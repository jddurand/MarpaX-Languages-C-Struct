#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 3;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Languages::C::Struct' ) || print "Bail out!\n";
}

my $cSourceCode = do { local $/; <DATA> };
my $b = MarpaX::Languages::C::Struct->new();
$b->parse($cSourceCode);
my $ntypedef = $b->typedefs;
ok(defined($ntypedef), "There are $ntypedef typedefs at file-scope level");
my @typedef = $b->typedefs;
my $nenum = $b->enums;
ok(defined($nenum), "There are $nenum enums");
my @enum = $b->enums;
use Data::Dumper;
print Dumper($b->ast);
__DATA__
#include <stdlib.h>
