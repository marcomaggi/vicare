#!/usr/bin/perl -w

use strict;

sub average {
  my $n = 0;
  my $s = 0;
  $n++, $s+=$_ foreach @_;
  $s/$n;
}

sub min {
  my $m = shift;
  ($_ < $m) and $m = $_ foreach @_;
  $m;
}

sub max {
  my $m = shift;
  ($_ > $m) and $m = $_ foreach @_;
  $m;
}



my %times;
my %benchmarks;
my %runtimes;
my %gctimes;

{
  my $curtime;
  my $curbench;
  my $counter = 0;
  open F, "<timelog" or die;
  while(<F>){
    if (/^NOW: (.*)/){
      $curtime = $1;
      $times{$curtime} = ++$counter;
      next;
    }
    if(/^running stats for (.*):$/){
      $curbench = $1;
      $benchmarks{$curbench} ||= ++$counter;
      next;
    }
    if(/^ *(\d*) ms elapsed cpu time, including (\d*) ms collecting$/){
      push @{$runtimes{$curbench}{$curtime}}, $1;
      push @{$gctimes{$curbench}{$curtime}}, $2;
      next;
    }
  }
  close F;
}

my @times = sort { $times{$a} <=> $times{$b} } keys %times;
my @benchmarks = sort { $benchmarks{$a} <=> $benchmarks{$b} } keys %benchmarks;

my $verbose = 0;

foreach my $bench (@benchmarks){
  print "benchmark: $bench\n";
  my $prev = 0;
  foreach my $time (@times){
    defined $runtimes{$bench}{$time} or next;
    my @times = @{$runtimes{$bench}{$time}};
    my $t = min(@times);
    if($prev){
      my $diff = (($t - $prev) / $prev) * 100;
      printf "   %6d %6s             on $time\n", 
        $t,
        sprintf("(%s%d%%)", ($diff>0) ? "+" : ($diff<0) ? "-" : "", abs $diff);
    } else {
      printf "   %6d                    on $time\n", $t;
    }
    $prev = $t;
  }
}


