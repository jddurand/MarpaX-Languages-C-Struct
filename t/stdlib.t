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
my $typedefs = $b->typedefs;
ok(defined($typedefs), "There are " . int(keys %{$typedefs}) . " typedef at file-scope level: " . join(', ', keys %{$typedefs}));
my $enums = $b->enums;
ok(defined($typedefs), "There are " . int(keys %{$enums}) . " enum at file-scope level: " . join(', ', keys %{$enums}));
use Data::Dumper;
print Dumper($b->{_value});
$b->resolveType('myfunc');
__DATA__
typedef struct mystruct_ {
    int i;
    char p[];
} s_mystruct_;
typedef char (*myfunc)(int i, double j);
/* #include <stdlib.h> */

