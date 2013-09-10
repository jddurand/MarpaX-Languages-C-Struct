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
use Config;
use Config::AutoConf;
use Carp qw/croak/;
use constant {
    CATEGORY_ONLY_NATIVE_TYPES => 0,
    CATEGORY_STRUCTORUNION     => 1,
    CATEGORY_ENUM              => 2,
};

use constant {
    LBRACKET                   => '[',
    STAR                       => '*',
};

my $E_FLAG = '-E';

# ABSTRACT: C structures dissection using Marpa

# VERSION

=head1 DESCRIPTION

This module is dissecting C structure, providing in particular L<pack>/L<unpack> helpers, and structure members accessors. It is inheriting from L<ExtUtils::CBuilder>, so please refer to this module for the new method.

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

my %SIZE = ();
my %SIGNED = ();

sub new {
  my $class = shift;

  my $self = $class->SUPER(@_);

  $self->{_ast} = undef;
  $self->{_value} = undef;
  $self->{_typedef} = {};

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

  my $output = $self->compile(source => $tmpc->filename, object_file => $tmpo->filename, extra_compiler_flags => $E_FLAG);

  open(my $txt, '<', $tmpo->filename);
  my $content = do { local $/; <$txt> };
  close($txt);
  $self->{_ast} = MarpaX::Languages::C::AST->new()->parse(\$content);
  $self->{_value} = ${$self->ast->value};

  return $self;
}

=head1 typedefs($self);

Get the list of typedefs at file-scope level. This is a reference to a hash with the typedef name as key, and user-data as value, user-data depending on MarpaX::Languages::C::AST, currently: the offset and length in the parsed input.

=cut

sub typedefs {
  my ($self) = @_;

  return $self->{_ast}->scope->typedefPerScope->[0];
}

=head1 ast($self);

Get the AST of the parsed source. This is a MarpaX::Languages::C::AST object.

=cut

sub ast {
  my ($self) = @_;

  return $self->{_ast};
}

=head1 value($self);

Get the AST parsed tree value. This is a tree of ARRAYs blessed to grammar's LHS.

=cut

sub value {
  my ($self) = @_;

  return $self->{_value};
}

=head1 enums($self);

Get the list of enums.  This is a reference to a hash with the enumeration name as key, and user-data as value, user-data depending on MarpaX::Languages::C::AST, currently: the offset and length in the parsed input.

=cut

sub enums {
  my ($self) = @_;

  return $self->{_ast}->scope->enumAnyScope;
}

=head1 resolveType($self, $type);

Converts a typedef to its original type, i.e. a native type or an aggregate (i.e. struct) type. Conversion is limited to file-scope translation units of C grammar. Supported entries for $type is either a typedef, a native type, or something in the form 'struct xxx'.

=cut

sub resolveType {
  my ($self, $type) = @_;

  return $self->_resolveType($type);
}

# ---------------------------
# PRIVATE METHODS/SUBROUTINES
# ---------------------------

sub _resolveType {
  my ($self, $type) = @_;

  if (grep {$_ eq $type} keys %{$self->typedefs}) {
      return $self->_resolveTypedef($type);
  } elsif ($type =~ /\s*struct\s+(\w+)/) {
      return $self->_resolveStruct($type);
  } else {
      return $self->_resolveNativeType($type);
  }
}

#
# This routine resolve the full declarator starting from a directDeclaratorIdentifier.
# Parent of DirectDeclaratorIdentifier is a recursive list of declarator and directDeclarator.
#
sub _resolveDeclarator {
    my ($self, $directDeclaratorIdentifierAddr, $identifier) = @_;

    my $childAddr = $directDeclaratorIdentifierAddr;
    my $parentAddr = undef;
    my $parent = undef;
    do {
	if (MarpaX::Languages::C::AST::Util::Data::Find->new
	    (
	     wanted => sub {
		 my ($self, $childAddr, $o) = @_;
		 my $rc = 0;
		 if (blessed($o) || ref($o) eq 'ARRAY') {
		     foreach (@{$o}) {
			 if (! defined($_)) {
			     next;
			 }
			 my $refaddr = refaddr($_);
			 if (defined($refaddr) && $refaddr == $childAddr) {
			     $rc = 1;
			     last;
			 }
		     }
		 }
		 return $rc;
	     },
	     wantedArgs => [ $self, $childAddr ],
	     callback => sub {
		 my ($self, $childAddr, $parentAddrp, $parentp, $o) = @_;
		 if ($self->_wantBlessed('C::AST::directDeclarator', $o) ||
		     $self->_wantBlessed('C::AST::declarator', $o)) {
		     ${$parentAddrp} = refaddr($o);
		     ${$parentp} = $o;
		 } else {
		     ${$parentAddrp} = undef;
		 }
	     },
	     callbackArgs => [ $self, $childAddr, \$parentAddr, \$parent ],
	    )->process($self->value)) {
	    if (! defined($parentAddr)) {
		$parentAddr = $childAddr;
		$childAddr = undef;
	    } else {
		$childAddr = $parentAddr;
	    }
	} else {
	    $parentAddr = $childAddr;
	    $childAddr = undef;
	}
    } while (defined($childAddr));
    if (! defined($parentAddr)) {
	croak "Cannot get top declarator of $identifier";
    }
    #
    # Build the full declarator
    #
    my $fullDeclarator = '';
    my $startIndexInFullDeclarator = undef;
    MarpaX::Languages::C::AST::Util::Data::Find->new
	(
	 callback => sub {
	     my ($self, $fullDeclaratorp, $startIndexInFullDeclaratorp, $o) = @_;
	     if (defined($o) && blessed($o) && refaddr($o) == $directDeclaratorIdentifierAddr) {
		 #
		 # Next entry will the IDENTIFIER
		 # Putting it to a negative value is one way from several to
		 # identify it has been initialized /now/.
		 $startIndexInFullDeclarator = -length(${$fullDeclaratorp});
	     }
	     if (ref($o) eq 'ARRAY') {
		 if ($o->[2] =~ /^\w/ && ${$fullDeclaratorp} =~ /\w$/) {
		     ${$fullDeclaratorp} .= ' ';
		     if (defined($startIndexInFullDeclarator) && $startIndexInFullDeclarator < 0) {
			 #
			 # Has just been initialized before
			 #
			 $startIndexInFullDeclarator--;
		     }
		 }
		 if (defined($startIndexInFullDeclarator) && $startIndexInFullDeclarator < 0) {
		     $startIndexInFullDeclarator = -$startIndexInFullDeclarator;
		 }
		 ${$fullDeclaratorp} .= $o->[2];
	     }
	 },
	 callbackArgs => [ $self, \$fullDeclarator, \$startIndexInFullDeclarator ],
	)->process($parent);

    return ($fullDeclarator, $startIndexInFullDeclarator);
}

#
# This routine searches for a directDeclaratorIdentifier ::= IDENTIFIER whose data
# is the offset and length of a typedef
#
sub _resolveTypedef {
    my ($self, $typedef) = @_;

    if (! defined($self->{_typedef}->{$typedef})) {
	my $data = $self->typedefs->{$typedef};
	my ($offset, $length) = @{$data};

	#
	# Search this entry in the AST. Per def this is a directDeclaratorIdentifier.
	#
	my $directDeclaratorIdentifierAddr = 0;
	if (! MarpaX::Languages::C::AST::Util::Data::Find->new
	    (
	     wanted => sub {
		 my ($self, $offset, $length, $o) = @_;
		 my $rc = 0;
		 if ($self->_wantBlessed('C::AST::directDeclaratorIdentifier', $o)) {
		     my $IDENTIFIER = $o->[0];
		     $rc = ($IDENTIFIER->[0] == $offset && $IDENTIFIER->[1] == $length);
		 }
		 return $rc;
	     },
	     wantedArgs => [ $self, $offset, $length ],
	     callback => sub {
		 my ($self, $directDeclaratorIdentifierAddrp, $o) = @_;
		 ${$directDeclaratorIdentifierAddrp} = refaddr($o);
	     },
	     callbackArgs => [ $self, \$directDeclaratorIdentifierAddr ],
	    )->process($self->value)) {
	    croak "Cannot find typedef $typedef in the AST parse tree value";
	}

	my ($fullDeclarator, $startIndexInFullDeclarator) = $self->_resolveDeclarator($directDeclaratorIdentifierAddr, $typedef);

	print "Typedef resolving of      : $typedef\n";
	print "Full declarator           : $fullDeclarator\n";
	print "startIndexInFullDeclarator: $startIndexInFullDeclarator\n";

	$self->{_typedef}->{$typedef} = [ fullDeclarator => $fullDeclarator,
					  startIndexInFullDeclarator => $startIndexInFullDeclarator ];
    } else {

	my ($fullDeclarator, $startIndexInFullDeclarator) = @{$self->{_typedef}->{$typedef}};
	print "Typedef resolving of (cached)      : $typedef\n";
	print "Full declarator (cached)           : $fullDeclarator\n";
	print "startIndexInFullDeclarator (cached): $startIndexInFullDeclarator\n";

    }
}

sub _wantBlessed {
  my ($self, $class, $o) = @_;

  return _blessed($o) eq $class;
}

sub _blessed {
  return blessed($_[0]) || '';
}

sub _resolveNativeType {
    my ($self, $type, $what) = @_;

    my $rc;
    my $unsigned = ($type =~ /\bunsigned\b/);
    my $signed = ($type =~ /\bsigned\b/);
    my $char = ($type =~ /\bchar\b/);

    if ($char && ! $signed && ! $unsigned) {
	#
	# Standard does not say if char should be signed or unsigned by default.
	# We must check it.
	#
	#my $tmp = File::Temp->new();
	#my $ac = Config::AutoConf->new(logfile => $tmp);
	#my $prologue = "#include <stddef.h>\n#include <stdio.h>\n#include <$header>";
	#my $program = "struct $struct t; t.$member = -1; printf(\"%d\\n\", t.$member < 0 ? 1 : 0);";
	#my $source = $ac->lang_build_program($prologue, $program);
	
    }

#    if (! exists($SIZE{$type})) {
#	my $ac = Config::AutoConf->new(logfile => $tmp);
#	$SIZE{$type} = $ac->check_sizeof_type($type);
#    }
#    if (! defined($SIZE{$type})) {
#	croak "Cannot determine size of $type";
#    }

}

=head1 NOTES

This module is logging via L<Log::Any>.

You will need a working compiler, and the default compiler/options are those with which perl was compiled. We make the assumption that the underlying compiler supports the -E flag (which is always the case AFAIK, even with cl compiler). Please overwrite this package's constant $E_FLAG if this is not the case with your compiler. 

There is NO attempt to generate pack()/unpack() helpers for something that perl does not know how to pack. This comment is adressing in particular complex numbers, atomic type specifiers (nothing prevents an _Atomic(int), for example, to be larger than int, inclusing space for a mutex, a guard, etc...).

The pack()/unpack() generated helpers produced will refuse to deal with unions unless the caller is providing a hook callback to tell which part of the union is concerned.

Compiler specific builtin-types are supported only when they resume to a keyword that can be immediately translated to a native type with no interpretation. I.e. gcc extension typeof() and __builtin_va_list are not supported. But MSVS extensions __int8, __int16, __int32 and __int64 are suppported.

Type specifiers supported are those that fall into three categories: they are all only on native types (including union promoted to int) except void, or this is a struct, or this is an union (in which case a userspace callback will be required to perform the pack()/unpack() helpers).

The signedness of a native char is always check if the 'unsigned' or 'signed' keyword is not explicitely present: the compiler might be told to apply unsigned behaviour by default, because ANSI C do not require a specific implementation of the char type. that is, 'char', 'signed char' and 'unsigned char' can be considered as three different types. So gcc have -funsigned-char and -fsigned-char options. MSVS have option /J.

=cut

1;
__DATA__
#
# This function gets the full declarator, i.e. a declarator that is not part
# of another declarator.
# First we search for the identifier, and then go up until the parent is not
# anymore a declarator.
#

sub _cdecl {
    my ($self, $what, $start) = @_;

    my $cdecl = '';

    #
    # First, find where is the directDeclaratorIdentifier hosting $what
    #
    my $directDeclaratorIdentifier = undef;

    MarpaX::Languages::C::AST::Util::Data::Find->new
	(
	 wanted => \&_wantBlessed,
	 wantedArgs => [ $self, 'C::AST::directDeclaratorIdentifier' ],
	 callback => sub {
	     my ($self,
		 $directDeclaratorIdentifierp,
		 $directDeclaratorIdentifier) = @_;

	     if ($directDeclaratorIdentifier->[2] eq $what) {
		 ${$directDeclaratorIdentifierp} = refaddr($directDeclaratorIdentifier);
	     },
	     callbackArgs => [ $self, \$directDeclaratorIdentifier ],
	 )->process($start);
	);
    if (! defined($directDeclaratorIdentifier)) {
	croak "directDeclaratorIdentifier of $what not found";
    }

    #
    # From now on, the parents of interest are:
    # - directDeclarator
    #   * will add eventually []
    # - declarator
	     
					      $typeSpecifiersp,
					      $pointersp,
					      $bracketsp,
					      $declp,
					      $externalDeclaration,
					      $declarationCheck,
					      $storageClassSpecifierTypedef,
					      $declarator) = @_;

					  MarpaX::Languages::C::AST::Util::Data::Find->new
					      (
					       wanted => \&_wantBlessed,
					       wantedArgs => [ $self, 'C::AST::pointer' ],
					       callback => sub {
						   my ($self, $pointersp, $o) = @_;

						   ${$pointersp}++;
					       },
					       callbackArgs => [ $self, $pointersp ],
					      )->process($declarator);

					  my $directDeclaratorFoundAddr = undef;
					  MarpaX::Languages::C::AST::Util::Data::Find->new
					      (
					       wanted => \&_wantBlessed,
					       wantedArgs => [ $self, 'C::AST::directDeclarator' ],
					       callback => sub {
						   my ($self,
						       $typeSpecifiersp,
						       $pointersp,
						       $bracketsp,
						       $declp,
						       $externalDeclaration,
						       $declarationCheck,
						       $storageClassSpecifierTypedef,
						       $declarator,
						       $directDeclaratorFoundAddrp,
						       $directDeclarator) = @_;
						   if ($self->_wantBlessed('C::AST::directDeclaratorIdentifier', $directDeclarator->[0])) {
						       #
						       # directDeclarator ::= directDeclaratorIdentifier
						       #
						       my $directDeclaratorIdentifier = $directDeclarator->[0];
						       if ($directDeclaratorIdentifier->[0]->[2] eq $type) {
							   printf STDERR "Setting \${\$directDeclaratorFoundAddrp} to 0x%0x\n", refaddr($directDeclarator);
							   ${$directDeclaratorFoundAddrp} = refaddr($directDeclarator);
							   #
							   # We found it: the resolved type is the aggregation of
							   # typeSpecifier(s) under
							   # declarationCheckdeclarationSpecifiers
							   # except storageClassSpecifierTypedef.
							   #
							   # Where is
							   # declarationCheckdeclarationSpecifiers ? This is the
							   # first member of :
							   # declarationCheck ::=
							   #  declarationCheckdeclarationSpecifiers
							   #  declarationCheckinitDeclaratorList
							   #  SEMICOLON
							   #
							   my $declarationCheckdeclarationSpecifiers = $declarationCheck->[0];
							   #
							   # declarationCheckdeclarationSpecifiers ::= declarationSpecifiers
							   #
							   my $declarationSpecifiers = $declarationCheckdeclarationSpecifiers->[0];
							   #
							   # declarationSpecifiers ::= declarationSpecifiersUnit+
							   #
							   foreach (@{$declarationSpecifiers}) {
							       my $declarationSpecifiersUnit = $_;
							       foreach (@{$declarationSpecifiersUnit}) {
								   next if (! $self->_wantBlessed('C::AST::typeSpecifier', $_));
								   my $typeSpecifier = $_;
								   push(@{$typeSpecifiersp}, @{$typeSpecifier});
							       }
							   }
						       }
						   }
					       },
					       callbackArgs => [ @_, \$directDeclaratorFoundAddr ],
					      )->process($declarator);

					  #
					  # Look for brackets. This can happen only if
					  # directDeclarator ::= directDeclarator xxx
					  #
					  if (defined($directDeclaratorFoundAddr)) {
					      my $directDeclaratorWantedAddr = $directDeclaratorFoundAddr;
					      my $directDeclaratorParentAddr = undef;
					      do {
						  $directDeclaratorParentAddr = undef;
						  MarpaX::Languages::C::AST::Util::Data::Find->new
						      (
						       wanted => \&_wantBlessed,
						       wantedArgs => [ $self, 'C::AST::directDeclarator' ],
						       callback => sub {
							   my ($self,
							       $typeSpecifiersp,
							       $pointersp,
							       $bracketsp,
							       $declp,
							       $externalDeclaration,
							       $declarationCheck,
							       $storageClassSpecifierTypedef,
							       $declarator,
							       $directDeclaratorWantedAddr,
							       $directDeclaratorParentAddrp,
							       $directDeclarator) = @_;
							   if ($directDeclaratorWantedAddr == refaddr($directDeclarator->[0])) {
							       #
							       # directDeclarator ::= directDeclarator xxx
							       #
							       if (! blessed($directDeclarator->[1]) &&
								   $directDeclarator->[1]->[2] eq LBRACKET) {
								   ${$bracketsp}++;
							       }
							       ${$directDeclaratorParentAddrp} = refaddr($directDeclarator);
							   }
						       },
						       callbackArgs => [ @_, $directDeclaratorWantedAddr, \$directDeclaratorParentAddr ],
						      )->process($declarator);
						  if (defined($directDeclaratorParentAddr)) {
						      $directDeclaratorWantedAddr = $directDeclaratorParentAddr;
						  }
					      } while (defined($directDeclaratorParentAddr));
					  }
				      },
				      callbackArgs => [ @_ ],
				     )->process($declarationCheck);
}

