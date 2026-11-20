#!/usr/bin/perl
## 001j  EXPORT FOR REVIEW  -  step 001, substep j: VALIDATION.
# Export located statement text to a folder a HUMAN can navigate, with an index.
#
#   perl 001j-export-for-review.pl <chunks.jsonl> <outdir> [name-pattern]
#
# With a name pattern it exports every chunk whose company name matches (case
# insensitive). With no pattern it exports everything, which is ~5 MB of text.
#
# Each file is named   <COMPANY>__<FY-date>__<form>__<statement>.txt
# so sorting the folder groups a filing's three statements together, and the
# company is the first thing you read. Every file carries a header block naming
# the filing, the accession number, the EDGAR URL, and which statement the
# locator was ASKED for - so a wrong block is visible on the first line instead
# of having to be inferred.
#
# Writes INDEX.txt listing every file exported, newest filing first.
use strict; use warnings;

my ($CH, $OUT, $PAT) = @ARGV;
die "usage: $0 <chunks.jsonl> <outdir> [name-pattern]\n" unless $CH && $OUT;

my %KIND = (cf => 'CASH-FLOW', is => 'INCOME-STMT', bs => 'BALANCE-SHEET');

sub field { my ($ln, $k) = @_;
  my ($v) = $ln =~ /"\Q$k\E":"((?:[^"\\]|\\.)*)"/;
  return defined $v ? $v : '';
}
sub unesc { my ($t) = @_;
  $t =~ s/\\n/\n/g; $t =~ s/\\t/\t/g; $t =~ s/\\"/"/g; $t =~ s/\\\\/\\/g;
  return $t;
}
sub clean { my ($s) = @_;
  $s =~ s/[^A-Za-z0-9 .&-]//g; $s =~ s/\s+/ /g; $s =~ s/^ | $//g;
  $s =~ s/ /-/g; return substr($s, 0, 34);
}

mkdir $OUT unless -d $OUT;
open(my $fh, '<', $CH) or die "$CH: $!";
my (@idx, $n, $tot);
$n = $tot = 0;
while (my $ln = <$fh>) {
  $tot++;
  my $co = field($ln, 'coname');
  next if defined $PAT && length $PAT && $co !~ /$PAT/i;
  my $f  = field($ln, 'wrdsfname');
  my $k  = field($ln, 'kind');
  my $fm = field($ln, 'form');
  my $rd = field($ln, 'rdate');
  my $fd = field($ln, 'fdate');
  my $ck = field($ln, 'cik');
  next unless $f && $k;

  (my $acc = $f) =~ s{^.*/}{};  $acc =~ s/\.txt$//;
  (my $accn = $acc) =~ s/-//g;
  my $url = "https://www.sec.gov/Archives/edgar/data/$ck/$accn/$acc-index.htm";

  my $name = sprintf("%s__%s__%s__%s.txt", clean($co), ($rd || 'nodate'),
                     ($fm || 'noform'), ($KIND{$k} || uc $k));
  my $txt = unesc(field($ln, 'text'));

  open(my $o, '>', "$OUT/$name") or die "$OUT/$name: $!";
  print $o "=" x 78, "\n";
  printf $o "COMPANY            %s\n", $co;
  printf $o "FORM / PERIOD      %s  ending %s   (filed %s)\n", $fm, $rd, $fd;
  printf $o "CIK                %s\n", $ck;
  printf $o "ACCESSION          %s\n", $acc;
  printf $o "EDGAR              %s\n", $url;
  printf $o "STATEMENT WANTED   %s   <-- is the text below actually this?\n",
            ($KIND{$k} || $k);
  printf $o "WRDS PATH          %s\n", $f;
  print $o "=" x 78, "\n\n";
  print $o $txt;
  close $o;

  push @idx, { rd => ($rd || ''), co => $co, fm => $fm, k => $k,
               name => $name, url => $url };
  $n++;
}
close $fh;

@idx = sort { $b->{rd} cmp $a->{rd} || $a->{co} cmp $b->{co}
              || $a->{k} cmp $b->{k} } @idx;
open(my $ix, '>', "$OUT/INDEX.txt") or die;
printf $ix "Located statement text for review.  %d files, newest filing first.\n", $n;
printf $ix "Source: %s\n", $CH;
printf $ix "Pattern: %s\n\n", (defined $PAT && length $PAT) ? $PAT : "(everything)";
printf $ix "%-12s %-34s %-6s %-14s %s\n", "PERIOD", "COMPANY", "FORM",
           "STATEMENT", "FILE";
printf $ix "%s\n", "-" x 150;
for my $r (@idx) {
  printf $ix "%-12s %-34s %-6s %-14s %s\n", $r->{rd}, substr($r->{co}, 0, 34),
             $r->{fm}, ($KIND{$r->{k}} || $r->{k}), $r->{name};
}
close $ix;

print "scanned $tot chunks, exported $n\n";
print "  -> $OUT\n  -> $OUT/INDEX.txt\n";
