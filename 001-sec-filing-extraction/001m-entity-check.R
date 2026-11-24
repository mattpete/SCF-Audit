## ============================================================================
## 001m  WHOSE STATEMENT IS IT?                           step 001, substep m
## Reads each located block and asks whether the entity printed above the table
## is the filer the inventory says. Writes a flag per filing.
## In : extract-batches/*/c###.txt + manifest.csv
## Out: extract-status/entity_flags.csv   accession -> entity_mismatch, who
## ============================================================================
## THE ONE ERROR CLASS THE ARITHMETIC CANNOT SEE. Every other check in this
## pipeline tests a statement against itself: do the sections sum to the net
## change, does the cash balance roll forward. A statement belonging to the
## WRONG COMPANY passes all of them, because it is a real statement and it is
## internally consistent. The only tell is the name printed above the table.
##
## Measured on the first 1,000 located blocks: 54 name a company that is not
## the filer, and 52 of those are one filer - Alabama Gas Corp, a wholly-owned
## subsidiary whose 10-K and 10-Q yield its PARENT's consolidated statement
## (Energen 39, Spire 6, The Laclede Group 5, matching its three successive
## owners). Confirmed by reading one: the block for its FY2015 10-K carries the
## line "Acquisition of Alagasco (net of $12.1 cash acquired in 2014)" -
## Alagasco IS Alabama Gas Corp, appearing as an acquisition target inside the
## statement, so the statement is unambiguously the parent's.
##
## It is not only parents. An AMD filing yielded Spansion's statement and
## another yielded GLOBALFOUNDRIES'; a Cheniere 10-K yielded Freeport LNG
## Development's. Any filing that reprints another registrant's statements is
## exposed, which is why this flags by name rather than by filer type.
##
## A FLAG, NEVER A DROP. Whether a wholly-owned subsidiary's filings belong in
## the sample at all is a research-design decision, not something a script
## should make silently. So nothing is excluded here: the flag makes the
## population filterable and leaves the choice open.
##
## WHY A MISSING NAME IS NOT A MISMATCH. Most statements simply begin
## "CONSOLIDATED STATEMENTS OF CASH FLOWS" and never name the company. Treating
## that as evidence flagged 93 of 1,000 blocks instead of 54, i.e. mostly
## noise. This flags only a block that names a DIFFERENT company.
## ============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr)
})

SD <- Sys.getenv("SEC_DATA_PATH"); stopifnot(nzchar(SD))
BD <- paste0(SD, "/extract-batches")
OD <- paste0(SD, "/extract-status")
dir.create(OD, recursive = TRUE, showWarnings = FALSE)

## words that identify nobody - two filers sharing only these are not a match
STOP <- c("inc", "inc.", "corp", "corp.", "corporation", "company", "co", "co.",
          "the", "group", "holdings", "holding", "ltd", "llc", "lp", "l.p.",
          "plc", "and", "of", "international", "industries", "enterprises",
          "systems", "new", "&", "trust", "partners", "partnership")

key <- function(s) {
  w <- tolower(unlist(strsplit(gsub("[^A-Za-z& ]", " ", s), " +")))
  unique(w[nchar(w) > 1 & !w %in% STOP])
}

## an ALL-CAPS line that names a company, as opposed to a statement title
ENTITY <- "(INC|CORP|CORPORATION|COMPANY|GROUP|L\\.?P\\.?|LLC|HOLDINGS|TRUST)\\.?,?$"
TITLE   <- paste("STATEMENT|BALANCE|OPERATIONS|CASH FLOW|INCOME|EQUITY|TOTAL",
                 "LIABILITIES|ASSETS|COMPREHENSIVE|UNAUDITED|CONTINUED", sep = "|")
HEAD_N  <- 40   # the entity line sits at the top of the block, never deep in it

scan_batch <- function(b) {
  mf <- paste0(BD, "/", b, "/manifest.csv")
  if (!file.exists(mf)) return(NULL)
  man <- read_csv(mf, show_col_types = FALSE, col_types = cols(.default = col_character()))
  bind_rows(lapply(seq_len(nrow(man)), function(i) {
    f <- sprintf("%s/%s/c%03d.txt", BD, b, as.integer(man$idx[i]))
    if (!file.exists(f)) return(NULL)
    ln  <- trimws(readLines(f, warn = FALSE, n = HEAD_N))[-1]
    ent <- unique(ln[grepl("^[A-Z][A-Z&.,'’ -]{5,}$", ln) &
                       grepl(ENTITY, ln) & !grepl(TITLE, ln)])
    hdr <- key(man$coname[i])
    ## any named entity sharing a distinctive word with the filer => it is theirs
    ok  <- !length(ent) ||
      any(vapply(ent, function(e) length(intersect(key(e), hdr)) > 0, logical(1)))
    tibble(batch = b, idx = as.integer(man$idx[i]),
           accession = man$accession[i], cik = man$cik[i],
           filer = man$coname[i], form = man$form[i], fdate = man$fdate[i],
           entity_named    = if (length(ent)) paste(ent, collapse = " / ") else NA_character_,
           entity_mismatch = !ok)
  }))
}

batches <- list.dirs(BD, full.names = FALSE, recursive = FALSE)
batches <- batches[nzchar(batches)]
r <- bind_rows(lapply(batches, scan_batch))
if (!nrow(r)) stop("no batch text found under ", BD)
write_csv(r, paste0(OD, "/entity_flags.csv"))

cat("\n=== whose statement is it? ", nrow(r), " located blocks ===\n\n", sep = "")
print(table(mismatch = r$entity_mismatch))
cat(sprintf("\nnames a company that is not the filer: %d (%.1f%%)\n",
            sum(r$entity_mismatch), 100 * mean(r$entity_mismatch)))

bad <- r |> filter(entity_mismatch)
if (nrow(bad)) {
  cat("\n--- by filer, and whose statement turned up instead ---\n")
  print(as.data.frame(bad |> count(filer, entity_named, sort = TRUE)), row.names = FALSE)
  cat("\nThese are flagged, NOT dropped. Join entity_flags.csv on accession to\n")
  cat("exclude them, and decide separately whether a wholly-owned subsidiary's\n")
  cat("filings belong in the sample at all.\n")
}
cat("\nwrote ", OD, "/entity_flags.csv\n", sep = "")
