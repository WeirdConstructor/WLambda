# Syntax Highlight Test File

# Comments

# Comments with TODO FIXME XXX
# But ok with identifiers:
TODO FIXME XXX

# Identifiers:
foß@o?-f<>o&/fofo
f+-@$%&/_-:*!"'f
@fooo
_foo
_?foo
?F
FOOF*X
?FOF?
`FOO`
foo$@intbar

# Numbers:
-1234567890 -0 -1 +12349 -1r1234567890 -99r3204 324
-1r1 +4r3 1r1 9r9 99r9
0x01234567890ABCDEF
0x01234567890ABCDEF0
0b010101011 0b0101.01011
0o0234      0o023.324       0o0324.234
0xFFFFEE    0xFF.FF         0xFF.FF0
2r3.3

# Non Numbers (Syntax errors):
0o9 0o39 0xQ 0x0Q 0b02 0b2
0b0.2 0b0.f 0b0234 0xFFFFEEQ

# Symbols:
:foo
:"bar"
:@FOO
:+X+
:`F`

# Multiline symbol:
:"foo x x \f \"
bar"

# Strings (with identifiers before and after):
x "ABC" x
x "A\"C" x
x "A\\\"C" x
x "A\t\n\r\0\u{FFAA}\"\'\\\xFF\x93C" x

# Byte Strings:
x $b"ABC" x
x $b"A\"C" x
x $b"A\\\"C" x
x $b"A\t\n\r\0\u{FFAA}\"\'\\\xFF\x93C" x

# String non escape errors:
x "A\u0302" x

# Quoted Strings/Byte vectors/regex patterns/selectors:
x $q fofoof x
O $q(feofw) x
O $q(f[fef]eofw) x
O $q(f[f{ef}]eofw) x
o $q[fofeofeo] ofe
o $q{fofeofeo} ofe
o $q<fo))}[}¼¼}³feofeo> ofe
x $Q fofoof x
O $Q(feofw) x
O $Q(f[fef]eofw) x
O $Q(f[f{ef}]eofw) x
o $Q[fofeofeo] ofe
o $Q{fofeofeo} ofe
o $Q<fo))}[}¼¼}³feofeo> ofe
# Regexes:
x $r fofoof x
O $r(feofw) x
O $r(f[fef]eofw) x
O $r(f[f{ef}]eofw) x
o $r[fofeofeo] ofe
o $r{fofeofeo} ofe
o $r<fo))}[}¼¼}³feofeo> ofe

# Regex highlighting
o $r/ff|fefe*_?$*f$+fe\u{FFEE}$?X$=e$!(*F*)f\;;:"'(){} /
o $r!ff|fefe*_?$*f$+fe\u{FFEE}$?X$=e(*F*)f\;;:"'(){} !
o $r<XXEE>
o $r(XX  EE  )
o $r[(XX  EE  )]
o $r{XX  EE  } fef
o $r`
    ff | fefe * _ ?
    $*f $+f $?f $=f $!e
    $^ $$ $s $S $&U $&L
    \u{33ff} \xFF \: \' \; \$
    ( a b \xFFEe * ? )
    (^ foo\) \( )
    [abc\(\[\]\) \u{FFFF} ]
    [^fofe\(\[\]\) \xFF ]
    "

    Errors
    ,
    ! / ^
    ' & : ; $
    { } =
`

# Selectors:
x $S fofoof  x
x $S`a/b/**/fo*x :(*/f) & :{a=10}` x
x $S[a/b/**/fo*x :(*/f) & :{a=10}] x
x $S!a/b/**/fo*x :(*/f) & :{a=10}! x
O $S`
    feofw
    $*f
    :!type=X :type=X :str=Y :!str=Y / feo
    :{ x = y, foo = X } :!{ x = y }
    :(a / b / c)        :!(a / b / c)
    **
    ^
    afe / fefe

    Errors
    ! = , { }
` x

# Wrongly quoted stuff seen as such:
O $q(f[f{e(f)}]eofw) x
O $q(f(fef)eofw) x

# Wrongly quoted stuff seen as such:
O $q(f[f{e(f)}]eofw) x
O $q(f(fef)eofw) x

# Structure Match:
$P $[a, f, b]

# Some global values:
$self $s
$data $d
$\

# Some values:
$true   $t
$false  $f
$none   $n

$error $[123,:foo]
$e     $[123,:foo]

# References:
$& fef
$&& fef
# Capture ref:
$:fo
# Deref
$*fo

# Accumulators
$@i $@int
$@f $@float
$@b $@bytes
$@v $@vec
$@m $@map
$@s $@string
$@a $@accum
$+


# Code Strings
$code{ .x = 10 }
$c{ .x = 10 }

# Maps:
${ a = $q{bar}, x = $[ 1, 2, 3 ],
    o = { function here; .f = 3; !x = 32 },
    f = \.x = 30, k = \!foo = 32
}

# Vectors:
$[ a 3, ${ $q{x} = 300, e = $i(1,2,3) }, { function here }, \.x = _ ]

# Splices:
$[a, *fe]
${a = 3, *x}

# Number vectors:
$f(1,2)
$f(1,2,3)
$f(1,2,3,4)
$i(1,2)
$i(1,2,3)
$i(1,2,3,4)

# Pairs:
$p($true, 20)
$p($p(foo),$p($@int))
# Pair constructor:
a => b

# Export Import
!@export std = 20
!@import std std
!@import std
!@wlambda

# Function combinators:
a b | c d e
a b || c d e
a b |> c d e
x y <& f e e o
x y &> f e e o

# Function arity
{||     X }
{|0|    X }
{|0<2|  X }

# Function label:
\:foo {
    \:fo { }
}
