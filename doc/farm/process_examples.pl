#!/usr/bin/perl -w

use strict;

my $n = 0;

sub process {
    my $pattern = shift @_;
    my $grid = shift @_;
    $pattern =~ s/^[\n\s]+//sg;
    my $fn = "images/example-$n";
    $n++;
    my $result = qq!\\begin{center}
\\begin{lstlisting}
$pattern
\\end{lstlisting}
\\includegraphics[width=0.45\\textwidth]{$fn.pdf}
\\end{center}!;
    open(my $fh, ">tmp.hs");
    print($fh "import Sound.Tidal.Vis
import qualified Graphics.Rendering.Cairo as C 
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Sound.Tidal.Context
vis pat = vLines (C.withSVGSurface) \"$fn.svg\" (400,40) pat 1 1
visgrid pat = vLines (C.withSVGSurface) \"$fn.svg\" (400,400) pat 10 20
");
    my $func = $grid ? "visgrid" : "vis";
    print($fh ("main = $func \$ " . $pattern . "\n"));
    close $fh;

    system("runhaskell -XOverloadedStrings tmp.hs");
    
    open(my $svgfh, "<$fn.svg")
	or die "couldn't open $fn.svg: $!";
    my $svg;
    {
	local $/;
	undef $/;
	$svg = <$svgfh>
    }
    close $svgfh;
    $svg =~ s!<svg\s+(.*?)>!<svg $1 shape-rendering="crispEdges">!s;
    
    open($svgfh, ">$fn.svg")
	or die "couldn't open $fn for writing: $!";
    print $svgfh $svg;
    close($svgfh);
    system("convert $fn.svg $fn.pdf");
    return($result);
}

open(my $in, "<farm.tex")
    or die "couldn't open farm.tex: $!";
my $stuff;
{
    local $/;
    undef $/;
    $stuff = <$in>
}
close($in);
open(my $out, ">farm_processed.tex");

$stuff =~ s!<example>(.*?)</example>!process($1, 0)!sge;
$stuff =~ s!<examplegrid>(.*?)</examplegrid>!process($1, 1)!sge;

print $out $stuff;
close($out);
