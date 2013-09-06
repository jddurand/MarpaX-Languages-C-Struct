use strict;
use warnings FATAL => 'all';

package MarpaX::Languages::C::Struct;
use MarpaX::Languages::C::AST;
use MarpaX::Languages::C::AST::Util::Data::Find;
use parent qw/ExtUtils::CBuilder/;
use Log::Any qw/$log/;
use File::Temp;
use SUPER;
use Scalar::Util qw/blessed refaddr/;
use IO::Handle;
use Config;

autoflush STDOUT 1;

# ABSTRACT: C structures dissection using Marpa

# VERSION

=head1 DESCRIPTION

This module is dissecting C structure, providing in particular L<pack>/L<unpack> helpers, and structure members accessors. It is inheriting from L<ExtUtils::CBuilder>, so please refer to this module for the new method. This module makes the assumption that the underlying compiler supports the -E flag (which is always the case AFAIK, even with cl compiler).

=head1 SYNOPSIS

    use MarpaX::Languages:C::Struct;

    my %config = ();          # C.f. ExtUtils::CBuilder->new(%config)
    my $b = MarpaX::Languages::C::Struct->new(%config);
    $b->parse("#include <stdlib.h>")
    my $ast = $b->value;

=head1 SUBROUTINES/METHODS

=head1 new(%config)

This package is using ExtUtils::CBuilder as parent, please refer to L<ExtUtils::CBuilder> documentation.

=cut

sub new {
  my $class = shift;

  my $self = $class->SUPER(@_);

  $self->{_ast} = undef;

  return $self;
}

=head1 parse($self, $source)

Parses the C source $source. The underlying module MarpaX::Languages::C::AST will croak in case of failure. Returns $self;

=cut

sub parse {
  my ($self, $source) = @_;

  my $tmpc = File::Temp->new( UNLINK => 0, SUFFIX => '.c' );
  print $tmpc $source;
  close($tmpc);

  my $tmpo = File::Temp->new( UNLINK => 0, SUFFIX => '.txt' );
  close($tmpo);

  my $output = $self->compile(source => $tmpc->filename, object_file => $tmpo->filename, extra_compiler_flags => '-E');

  open(my $txt, '<', $tmpo->filename);
  my $content = do { local $/; <$txt> };
  close($txt);
  $self->{_ast} = MarpaX::Languages::C::AST->new()->parse(\$content);

  return $self;
}

=head1 typedefs($self);

Get the list of typedefs at file-scope level. In array context, returns the list of typedefs. In scalar context, return the number of typedefs.

=cut

sub typedefs {
  my ($self) = @_;

  return keys %{$self->{_ast}->scope->typedefPerScope->[0]};
}

=head1 ast($self);

Get the AST of the parsed source. This is a MarpaX::Languages::C::AST object.

=cut

sub ast {
  my ($self) = @_;

  return $self->{_ast}->value;
}

=head1 enums($self);

Get the list of enums. In array context, returns the list of enums. In scalar context, return the number of enums.

=cut

sub enums {
  my ($self) = @_;

  return keys %{$self->{_ast}->scope->enumAnyScope};
}

=head1 typedef2type($self);

Converts a typedef to its original type, i.e. a native type or an aggregate (i.e. struct) type. Conversion is limited to file-scope translation units of C grammar.

=cut

sub typedef2type {
  my ($self) = @_;

  return keys %{$self->{_ast}->{_scope}->typedefPerScope->[0]};
}

=head1 struct2stubs($self, $structname)

Returns an array containing two anonymous stubs that will pack and unpack structure $structname, respectively . These anonymous stubs will have the same prototype as pack()/unpack(). Will croak if there is an error.

=cut

sub struct {
  my ($self) = @_;

  #
  # Because translationUnit can contain translationUnit
  #
  MarpaX::Languages::C::AST::Util::Data::Find->new
      (
       wanted => \&_wanted,
       wantedArgs => [ $self, 'C::AST::externalDeclaration' ],
       callback => \&_callback1,
       callbackArgs => [ $self ],
      )->process(${$self->{_ast}});

  return $self;
}

#
# PRIVATE METHODS/SUBROUTINES
#

#
# We want an externalDeclaration where one of its descendant is a structOrUnionSpecifier, itself
# having a structDeclarationList as descendant.
#
sub _wanted {
  my ($self, $class, $o) = @_;

  return _blessed($o) eq $class;
}

sub _callback1 {
  my ($self, $o) = @_;

  MarpaX::Languages::C::AST::Util::Data::Find->new
      (
       wanted => \&_wanted,
       wantedArgs => [ $self, 'C::AST::structOrUnionSpecifier' ],
       callback => \&_callback2,
       callbackArgs => [ $self, $o ],
      )->process($o);
}

sub _callback2 {
  my ($self, $o1, $o) = @_;

  MarpaX::Languages::C::AST::Util::Data::Find->new
      (
       wanted => \&_wanted,
       wantedArgs => [ $self, 'C::AST::structDeclarationList' ],
       callback => \&_callback3,
       callbackArgs => [ $self, $o1, $o ],
      )->process($o);
}

sub _callback3 {
  my ($self, $o1, $o2, $o) = @_;

  use Data::Dumper;
  print "==> " . refaddr($o) . "\n" . Dumper($o1);
}

sub _blessed {
  return blessed($_[0]) || '';
}

=head1 NOTES

This module is logging via L<Log::Any>.

=cut

1;
