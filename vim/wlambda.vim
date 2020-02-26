" Vim syntax file
" Language: WLambda
" Last Change: 2019-07-15
" Author: Weird Constructor <weirdconstructor@gmail.com>
" Maintainer: Weird Constructor <weirdconstructor@gmail.com>

"if exists("b:current_syntax")
"  finish
"endif

syn match wlSymbol      ':[^[:space:]\.,;{}\[\]()~|="]\+'

syn match wlIdentifier  /[a-zA-Z_@]\+[^[:space:]\.,;{}\[\]()~|=]*/
syn match wlAssignId    /[a-zA-Z_@]\+[^[:space:]\.,;{}\[\]()~|=]*/ contained
syn match wlMapKeyId    /[a-zA-Z_@]\+[^[:space:]\.,;{}\[\]()~|=]*\s*\ze=/ contained

syn match wlComment /#.*$/

syn keyword wlRepeat    for while range return break next match
syn keyword wlKeyword   panic not block unwrap unwrap_err error_to_str
syn keyword wlKeyword   sym is_some is_none is_err is_map is_vec is_fun is_str is_sym
syn keyword wlKeyword   is_ref is_wref is_bool is_bytes is_float is_int len type to_drop
syn keyword wlKeyword   float bool int on_error
syn match wlKeyword "_?"

syn match wlKeyword "std:append"
syn match wlKeyword "std:assert"
syn match wlKeyword "std:assert_eq"
syn match wlKeyword "std:bytes:from_hex"
syn match wlKeyword "std:bytes:from_vec"
syn match wlKeyword "std:bytes:to_hex"
syn match wlKeyword "std:bytes:to_vec"
syn match wlKeyword "std:chrono:timestamp"
syn match wlKeyword "std:cmp:num:asc"
syn match wlKeyword "std:cmp:num:desc"
syn match wlKeyword "std:cmp:str:asc"
syn match wlKeyword "std:cmp:str:desc"
syn match wlKeyword "std:copy"
syn match wlKeyword "std:deser:csv"
syn match wlKeyword "std:deser:json"
syn match wlKeyword "std:deser:msgpack"
syn match wlKeyword "std:displayln"
syn match wlKeyword "std:drop"
syn match wlKeyword "std:dump_func"
syn match wlKeyword "std:fold"
syn match wlKeyword "std:hash:fnv1a"
syn match wlKeyword "std:io:file:append"
syn match wlKeyword "std:io:file:read"
syn match wlKeyword "std:io:file:read_text"
syn match wlKeyword "std:io:file:write_safe"
syn match wlKeyword "std:measure_time"
syn match wlKeyword "std:neg"
syn match wlKeyword "std:num:abs"
syn match wlKeyword "std:num:int_to_closed_open01"
syn match wlKeyword "std:num:int_to_open01"
syn match wlKeyword "std:num:int_to_open_closed01"
syn match wlKeyword "std:pop"
syn match wlKeyword "std:prepend"
syn match wlKeyword "std:push"
syn match wlKeyword "std:rand:split_mix64_new"
syn match wlKeyword "std:rand:split_mix64_new_from"
syn match wlKeyword "std:rand:split_mix64_next"
syn match wlKeyword "std:rand:split_mix64_next_closed_open01"
syn match wlKeyword "std:rand:split_mix64_next_open01"
syn match wlKeyword "std:rand:split_mix64_next_open_closed01"
syn match wlKeyword "std:re:map"
syn match wlKeyword "std:re:match"
syn match wlKeyword "std:re:replace_all"
syn match wlKeyword "std:ref_id"
syn match wlKeyword "std:ser:csv"
syn match wlKeyword "std:ser:json"
syn match wlKeyword "std:ser:msgpack"
syn match wlKeyword "std:set_ref"
syn match wlKeyword "std:shuffle"
syn match wlKeyword "std:sort"

syn match wlKeyword "str"
syn match wlKeyword "std:str:cat"
syn match wlKeyword "std:str:from_char_vec"
syn match wlKeyword "std:str:from_utf8"
syn match wlKeyword "std:str:from_utf8_lossy"
syn match wlKeyword "std:str:join"
syn match wlKeyword "std:str:len"
syn match wlKeyword "std:str:padl"
syn match wlKeyword "std:str:padr"
syn match wlKeyword "std:str:to_bytes"
syn match wlKeyword "std:str:to_char_vec"
syn match wlKeyword "std:str:to_lowercase"
syn match wlKeyword "std:str:to_uppercase"
syn match wlKeyword "std:str:write"
syn match wlKeyword "std:strengthen"
syn match wlKeyword "std:take"
syn match wlKeyword "std:to_drop"
syn match wlKeyword "std:to_no_arity"
syn match wlKeyword "std:to_ref"
syn match wlKeyword "std:uneg"
syn match wlKeyword "std:unshift"
syn match wlKeyword "std:weaken"
syn match wlKeyword "std:wlambda:version"
syn match wlKeyword "std:write_str"
syn match wlKeyword "std:writeln"
syn match wlKeyword "std:yay"

syn match wlValue     '\$s'
syn match wlValue     '\$self'
syn match wlValue     '\$d'
syn match wlValue     '\$data'
syn match wlValue     '\$t'
syn match wlValue     '\$true'
syn match wlValue     '\$f'
syn match wlValue     '\$false'
syn match wlValue     '\$n'
syn match wlValue     '\$nul'
syn match wlValue     '\$e'
syn match wlValue     '\$error'
syn match wlRefData   '$&'
syn match wlRefData   '$&&'
syn match wlRefData   '$\*'
syn match wlValue     '[-+]\?\d\+'
syn match wlValue     '[-+]\?\d\+\.\d\+'
syn match wlValue     '[-+]\?0x[a-fA-F0-9]\+\(\.[a-fA-F0-9]\+\)\?'
syn match wlValue     '[-+]\?0b[01]\+\(\.[01]\+\)\?'
syn match wlValue     '[-+]\?0o[0-8]\+\(\.[0-8]\+\)\?'
syn match wlValue     '[-+]\?[0-9]\+r[0-9a-zA-Z]\+\(\.[0-9a-zA-Z]\+\)\?'

syn match wlFuncCombinators '|\?[|~]'

syn match wlStringSpec  /\\x[a-f0-9A-F][a-f0-9A-F]/  contained
syn match wlStringSpec  /\\["'\\nrt0]/               contained
syn match wlStringSpec  /\\u[a-f0-9A-F]\+/           contained
"
syn region wlString     start="\$q\z(.\)" end="\z1"
syn region wlString     start="\$q\["     end="\]"
syn region wlString     start="\$q("      end=")"
syn region wlString     start="\$q{"      end="}"

syn region wlQSymbol    start=+:"+      skip=+\\\\\|\\"+  end=+"+ contains=wlStringSpec
syn region wlString     start=+"+       skip=+\\\\\|\\"+  end=+"+ contains=wlStringSpec
syn region wlString     start=+\$b"+    skip=+\\\\\|\\"+  end=+"+ contains=wlStringSpec

syn match wlAssign   '\(^\|\s\+\|{\|;\)\@<=\.' nextgroup=wlAssignId,wlDestr
syn match wlDefine   '!'                       nextgroup=wlImport,wlExport,wlDefTag,wlAssignId,wlDestr
syn match wlDefTag   ':global' skipwhite skipnl nextgroup=wlAssignId contained
syn match wlImport   '@import' skipwhite skipnl nextgroup=wlAssignId contained
syn match wlExport   '@export' skipwhite skipnl nextgroup=wlAssignId contained
syn region wlDestr         matchgroup=wlDestrDelim start="(" end=")" transparent contained contains=wlIdentifier,wlComment
syn match wlArity     '|\s*\d\+\s*<\s*\d\+\s*|'
syn match wlArity     '|\s*\d\+\s*|'
syn match wlArity     '|\s*|'

syn region wlMap       matchgroup=wlRefData start="\${" end="}"     transparent contains=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlQuote,wlShortFunc,wlMapKeyId
syn region wlList      matchgroup=wlRefData start="\$\[" end="\]"   transparent contains=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlQuote,wlShortFunc
syn region wlQuote     matchgroup=wlExprQuote start="(" end=")"     transparent contains=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlQuote,wlShortFunc
syn region wlArgList   matchgroup=wlFuncDelims start="\[" end="\]"  transparent contains=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlQuote,wlShortFunc
syn region wlFunc      matchgroup=wlFuncDelims start="{" end="}"    transparent contains=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlQuote,wlShortFunc,wlAssign,wlDefine,wlArity
syn match wlShortFunc  '\\'                                                    nextgroup=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlQuote,wlShortFunc,wlAssign,wlDefine,wlArity

hi def link wlKeyword           Function
hi def link wlKeyword2          Function

hi def link wlArgList           Function
hi def link wlFuncDelims        Function
hi def link wlShortFunc         Function
hi def link wlFuncCombinators   Function
hi def link wlExprQuote         Normal

hi def link wlRepeat        Repeat
hi def link wlAssign        Statement
hi def link wlDefine        Statement
hi def link wlDestrDelim    Statement

hi def link wlValue         Constant

hi def link wlError         PreProc
hi def link wlImport        PreProc
hi def link wlExport        PreProc
hi def link wlString        String
hi def link wlStringSpec    Special
hi def link wlSymbol        Special
hi def link wlQSymbol       Special
hi def link wlAssignId      Special
hi def link wlMapKeyId      Special
hi def link wlRefData       Structure
hi def link wlDefTag        Type
hi def link wlArity         Type
hi def link wlSymbolSpec    Constant
hi def link wlIdentifier    Normal
hi def link wlComment       Comment


"" no '.'
"setl iskeyword=33,35-39,42-45,47-58,60-90,94,95,97-122,126
"setl lispwords+=do-each,for,$define!
"
"syn match lalError "[^ `'\t\n()\[\]";]\+"
"syn match lalError "[)\]]"
"
"syn match lalTodo /TODO/    contained
"syn match lalTodo /FIXME/   contained
"syn match lalTodo /XXX/     contained
"
"syn region lalQuote matchgroup=lalData start=/'[`']*/ end=/[ \t\n()\[\]";]/me=e-1
"syn region lalQuote matchgroup=lalData start=/'['`]*"/ skip=/\\[\\"]/ end=/"/
"syn region lalQuote matchgroup=lalData start=/'['`]*|/ skip=/\\[\\|]/ end=/|/
"syn region lalQuote matchgroup=lalData start=/'['`]*#\?(/ end=/)/ contains=ALLBUT,lalQuasiquote,lalQuasiquoteForm,lalUnquote,lalForm,lalSyntax,lalFunction,lalKeyword,lalExtraSyntax,lalSyntaxSyntax,lalTypeSyntax,lalDatumCommentForm
"
"syn region lalQuasiquote matchgroup=lalData start=/`['`]*/ end=/[ \t\n()\[\]";]/me=e-1
"syn region lalQuasiquote matchgroup=lalData start=/`['`]*#\?(/ end=/)/ contains=ALLBUT,lalQuote,lalQuoteForm,lalForm,lalSyntax,lalFunction,lalKeyword,lalExtraSyntax,lalSyntaxSyntax,lalTypeSyntax,lalDatumCommentForm
"
"syn region lalUnquote matchgroup=lalParentheses start=/,/ end=/[ `'\t\n\[\]()";]/me=e-1 contained contains=ALLBUT,lalDatumCommentForm
"syn region lalUnquote matchgroup=lalParentheses start=/,@/ end=/[ `'\t\n\[\]()";]/me=e-1 contained contains=ALLBUT,lalDatumCommentForm
"syn region lalUnquote matchgroup=lalParentheses start=/,(/ end=/)/ contained contains=ALLBUT,lalDatumCommentForm
"syn region lalUnquote matchgroup=lalParentheses start=/,@(/ end=/)/ contained contains=ALLBUT,lalDatumCommentForm
"
"syn region lalQuoteForm      matchgroup=lalData start=/(/ end=/)/ contained contains=ALLBUT,lalQuasiquote,lalQuasiquoteForm,lalUnquote,lalForm,lalSyntax,lalFunction,lalKeyword,lalExtraSyntax,lalSyntaxSyntax,lalTypeSyntax,lalDatumCommentForm
"syn region lalQuasiquoteForm matchgroup=lalData start=/(/ end=/)/ contained contains=ALLBUT,lalQuote,lalForm,lalSyntax,lalFunction,lalKeyword,lalExtraSyntax,lalSyntaxSyntax,lalTypeSyntax,lalDatumCommentForm
"
""syn keyword lalSyntax @
""syn keyword lalSyntax @!
""syn keyword lalSyntax $
""syn keyword lalSyntax $!
""syn keyword lalSyntax $^!
""syn keyword lalSyntax @^!
"
"syn match lalIdentifier  /[a-zA-Z0-9!$%&*+\-/<=>?@^_~#]\+\([a-zA-Z0-9!$%&*+\-/:<=>?@^_~#]\+[a-zA-Z0-9!$%&*+\-/<=>?@^_~#]\+\)\?/
"syn match lalKeyword  /\(:[a-zA-Z0-9!$%&*+\-/:<=>?@^_~#]\+\|[a-zA-Z0-9!$%&*+\-/:<=>?@^_~#]\+:\>\)/
"
"syn region lalString          start=/\(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
"syn region lalString          start=+#q\z(.\)+    skip=+\\\z1+   end=+\z1+
"syn region lalMultilineString start=+#<#\z(\I\i*\).*+            end=+^\z1+
"syn region lalMultilineString start=+#<<\z(\I\i*\).*+            end=+^\z1+
"syn region lalSymbol          start=/\(\\\)\@<!|/ skip=/\\[\\|]/ end=/|/
"
"syn match lalNumber /\(#[dbeiox]\)*[+\-]*\([0-9]\|inf.0\|nan.0\)[0-9a-fA-F+\-@\.ilns]*/
"
"syn match lalBoolean /#t\(rue\)\?/
"syn match lalBoolean /#f\(alse\)\?/
"
"syn match lalEmptyList /nil/
"
"syn match lalCharacter /#\\.[^ `'\t\n\[\]()]*/
"syn match lalCharacter /#\\x[0-9a-fA-F]\+/
"
"syn match lalComment /;.*$/ contains=lalTodo
"syn match lalComment /#!.*$/ contains=lalTodo
"
"syn region lalMultilineComment start=/#|/ end=/|#/ contains=lalTodo,lalMultilineComment
"
"syn region lalForm matchgroup=lalParentheses start="(" end=")" contains=ALLBUT,lalUnquote,lalDatumCommentForm
"syn region lalForm matchgroup=lalParentheses start="\[" end="\]" contains=ALLBUT,lalUnquote,lalDatumCommentForm
"
"syn region lalVector matchgroup=lalData start="#(" end=")"          contains=ALLBUT,lalQuasiquote,lalQuasiquoteForm,lalUnquote,lalForm,lalSyntax,lalFunction,lalKeyword,lalExtraSyntax,lalSyntaxSyntax,lalTypeSyntax,lalDatumCommentForm
"syn region lalVector matchgroup=lalData start="#[fsu]\d\+(" end=")" contains=lalNumber,lalError
"
"syn region lalDatumComment matchgroup=lalDatumComment start=/#;[ \t\n`']*/ end=/[ \t\n()\[\]";]/me=e-1
"syn region lalDatumComment matchgroup=lalDatumComment start=/#;[ \t\n`']*"/ skip=/\\[\\"]/ end=/"/
"syn region lalDatumComment matchgroup=lalDatumComment start=/#;[ \t\n`']*|/ skip=/\\[\\|]/ end=/|/
"syn region lalDatumComment matchgroup=lalDatumComment start=/#;[ \t\n`']*\(#\([usf]\d\+\)\?\)\?(/ end=/)/ contains=lalDatumCommentForm,lalTodo
"syn region lalDatumCommentForm start="(" end=")" contained contains=lalDatumCommentForm,lalTodo
"
"syn keyword lalLibrarySyntax define-library
"syn keyword lalLibrarySyntax export
"syn keyword lalLibrarySyntax import
"syn keyword lalLibrarySyntax include
"syn keyword lalLibrarySyntax include-ci
"syn keyword lalLibrarySyntax include-library-declarations
"syn keyword lalLibrarySyntax library
"syn keyword lalLibrarySyntax only
"syn keyword lalLibrarySyntax prefix
"syn keyword lalLibrarySyntax rename
"syn keyword lalLibrarySyntax srfi
"syn keyword lalLibrarySyntax cond-expand
"
"syn keyword lalLibrarySyntax define-builtin
"syn keyword lalSyntaxSyntax define-syntax-translator
"
"syn keyword lalSyntaxSyntax define-syntax
"syn keyword lalSyntaxSyntax let-syntax
"syn keyword lalSyntaxSyntax letrec-syntax
"syn keyword lalSyntaxSyntax syntax-rules
"
"syn match lalSyntax /\(\$define\!\)/
"syn match lalSyntax /\.\.\?/
"syn match lalKeyword /\.\@<=[ ]*\([^ \[\]()\t\n]*\)/
"
"syn keyword lalSyntax =>
"syn keyword lalSyntax and
"syn keyword lalSyntax begin
"syn keyword lalSyntax case
"syn keyword lalSyntax case-lambda
"syn keyword lalSyntax cond
"syn keyword lalSyntax define
"syn keyword lalSyntax define-record-type
"syn keyword lalSyntax define-values
"syn keyword lalSyntax delay
"syn keyword lalSyntax delay-force
"syn keyword lalSyntax do
"syn keyword lalSyntax do-each
"syn keyword lalSyntax else
"syn keyword lalSyntax for
"syn keyword lalSyntax guard
"syn keyword lalSyntax if
"syn keyword lalSyntax lambda
"syn keyword lalSyntax let
"syn keyword lalSyntax let*
"syn keyword lalSyntax let*-values
"syn keyword lalSyntax let-values
"syn keyword lalSyntax letrec
"syn keyword lalSyntax letrec*
"syn keyword lalSyntax or
"syn keyword lalSyntax parameterize
"syn keyword lalSyntax quasiquote
"syn keyword lalSyntax quote
"syn keyword lalSyntax set!
"syn keyword lalSyntax unless
"syn keyword lalSyntax unquote
"syn keyword lalSyntax unquote-splicing
"syn keyword lalSyntax when
"syn keyword lalSyntax return
"syn keyword lalSyntax with-cleanup
"
"syn match lalFunction /\(@\!\?\)/
"syn keyword lalFunction *
"syn keyword lalFunction +
"syn keyword lalFunction -
"syn keyword lalFunction /
"syn keyword lalFunction <
"syn keyword lalFunction <=
"syn keyword lalFunction =
"syn keyword lalFunction >
"syn keyword lalFunction >=
"syn keyword lalFunction ?doc
"syn keyword lalFunction abs
"syn keyword lalFunction acos
"syn keyword lalFunction acos 
"syn keyword lalFunction angle
"syn keyword lalFunction append
"syn keyword lalFunction apply
"syn keyword lalFunction asin
"syn keyword lalFunction assoc
"syn keyword lalFunction assq
"syn keyword lalFunction assv
"syn keyword lalFunction atan
"syn keyword lalFunction binary-port?
"syn keyword lalFunction boolean=?
"syn keyword lalFunction boolean?
"syn keyword lalFunction bytevector
"syn keyword lalFunction bytevector-append
"syn keyword lalFunction bytevector-append 
"syn keyword lalFunction bytevector-copy
"syn keyword lalFunction bytevector-copy!
"syn keyword lalFunction bytevector-length
"syn keyword lalFunction bytevector-u8-ref
"syn keyword lalFunction bytevector-u8-set!
"syn keyword lalFunction bytevector?
"syn keyword lalFunction caaaar
"syn keyword lalFunction caaadr
"syn keyword lalFunction caaar
"syn keyword lalFunction caadar
"syn keyword lalFunction caaddr
"syn keyword lalFunction caadr
"syn keyword lalFunction caar
"syn keyword lalFunction cadaar
"syn keyword lalFunction cadadr
"syn keyword lalFunction cadar
"syn keyword lalFunction caddar
"syn keyword lalFunction cadddr
"syn keyword lalFunction caddr
"syn keyword lalFunction cadr
"syn keyword lalFunction call-with-current-continuation
"syn keyword lalFunction call-with-input-file
"syn keyword lalFunction call-with-output-file
"syn keyword lalFunction call-with-port
"syn keyword lalFunction call-with-values
"syn keyword lalFunction call/cc
"syn keyword lalFunction car
"syn keyword lalFunction cdaaar
"syn keyword lalFunction cdaadr
"syn keyword lalFunction cdaar
"syn keyword lalFunction cdadar
"syn keyword lalFunction cdaddr
"syn keyword lalFunction cdadr
"syn keyword lalFunction cdar
"syn keyword lalFunction cddaar
"syn keyword lalFunction cddadr
"syn keyword lalFunction cddar
"syn keyword lalFunction cdddar
"syn keyword lalFunction cddddr
"syn keyword lalFunction cdddr
"syn keyword lalFunction cddr
"syn keyword lalFunction cdr
"syn keyword lalFunction ceiling
"syn keyword lalFunction char->integer
"syn keyword lalFunction char-alphabetic?
"syn keyword lalFunction char-ci<=?
"syn keyword lalFunction char-ci<?
"syn keyword lalFunction char-ci=?
"syn keyword lalFunction char-ci>=?
"syn keyword lalFunction char-ci>?
"syn keyword lalFunction char-downcase
"syn keyword lalFunction char-foldcase
"syn keyword lalFunction char-lower-case?
"syn keyword lalFunction char-numeric?
"syn keyword lalFunction char-ready?
"syn keyword lalFunction char-upcase
"syn keyword lalFunction char-upper-case?
"syn keyword lalFunction char-whitespace?
"syn keyword lalFunction char<=?
"syn keyword lalFunction char<?
"syn keyword lalFunction char=?
"syn keyword lalFunction char>=?
"syn keyword lalFunction char>?
"syn keyword lalFunction char?
"syn keyword lalFunction close-input-port
"syn keyword lalFunction close-output-port
"syn keyword lalFunction close-port
"syn keyword lalFunction command-line
"syn keyword lalFunction complex?
"syn keyword lalFunction concat!
"syn keyword lalFunction concat
"syn keyword lalFunction cons
"syn keyword lalFunction cos
"syn keyword lalFunction current-error-port
"syn keyword lalFunction current-input-port
"syn keyword lalFunction current-jiffy
"syn keyword lalFunction current-output-port
"syn keyword lalFunction current-second
"syn keyword lalFunction delete-file
"syn keyword lalFunction denominator
"syn keyword lalFunction digit-value
"syn keyword lalFunction display
"syn keyword lalFunction displayln
"syn keyword lalFunction dynamic-wind
"syn keyword lalFunction emergency-exit
"syn keyword lalFunction environment
"syn keyword lalFunction eof-object
"syn keyword lalFunction eof-object?
"syn keyword lalFunction empty?
"syn keyword lalFunction eq?
"syn keyword lalFunction equal?
"syn keyword lalFunction eqv?
"syn keyword lalFunction error
"syn keyword lalFunction error-object-irritants
"syn keyword lalFunction error-object-message
"syn keyword lalFunction error-object?
"syn keyword lalFunction eval
"syn keyword lalFunction even?
"syn keyword lalFunction exact
"syn keyword lalFunction exact->inexact
"syn keyword lalFunction exact-integer-sqrt
"syn keyword lalFunction exact-integer?
"syn keyword lalFunction exact?
"syn keyword lalFunction exit
"syn keyword lalFunction exp
"syn keyword lalFunction expt
"syn keyword lalFunction features
"syn keyword lalFunction file-error?
"syn keyword lalFunction file-exists?
"syn keyword lalFunction finite?
"syn keyword lalFunction first
"syn keyword lalFunction floor
"syn keyword lalFunction floor-quotient
"syn keyword lalFunction floor-remainder
"syn keyword lalFunction floor/
"syn keyword lalFunction flush-output-port
"syn keyword lalFunction for-each
"syn keyword lalFunction force
"syn keyword lalFunction gcd
"syn keyword lalFunction get-environment-variable
"syn keyword lalFunction get-environment-variables
"syn keyword lalFunction get-output-bytevector
"syn keyword lalFunction get-output-string
"syn keyword lalFunction imag-part
"syn keyword lalFunction inexact
"syn keyword lalFunction inexact->exact
"syn keyword lalFunction inexact?
"syn keyword lalFunction infinite?
"syn keyword lalFunction input-port-open?
"syn keyword lalFunction input-port?
"syn keyword lalFunction integer->char
"syn keyword lalFunction integer?
"syn keyword lalFunction interaction-environment
"syn keyword lalFunction jiffies-per-second
"syn keyword lalFunction keyword->string
"syn keyword lalFunction last
"syn keyword lalFunction lcm
"syn keyword lalFunction length
"syn keyword lalFunction list
"syn keyword lalFunction list->string
"syn keyword lalFunction list->vector
"syn keyword lalFunction list-copy
"syn keyword lalFunction list-ref
"syn keyword lalFunction list-set!
"syn keyword lalFunction list-tail
"syn keyword lalFunction list?
"syn keyword lalFunction load
"syn keyword lalFunction log
"syn keyword lalFunction magnitude
"syn keyword lalFunction make-bytevector
"syn keyword lalFunction make-list
"syn keyword lalFunction make-parameter
"syn keyword lalFunction make-polar
"syn keyword lalFunction make-promise
"syn keyword lalFunction make-rectangular
"syn keyword lalFunction make-string
"syn keyword lalFunction make-vector
"syn keyword lalFunction map
"syn keyword lalFunction max
"syn keyword lalFunction member
"syn keyword lalFunction memq
"syn keyword lalFunction memv
"syn keyword lalFunction min
"syn keyword lalFunction modulo
"syn keyword lalFunction nan?
"syn keyword lalFunction negative?
"syn keyword lalFunction newline
"syn keyword lalFunction nil?
"syn keyword lalFunction not
"syn keyword lalFunction null-environment
"syn keyword lalFunction null?
"syn keyword lalFunction number->string
"syn keyword lalFunction number?
"syn keyword lalFunction numerator
"syn keyword lalFunction odd?
"syn keyword lalFunction open-binary-input-file
"syn keyword lalFunction open-binary-output-file
"syn keyword lalFunction open-input-bytevector
"syn keyword lalFunction open-input-file
"syn keyword lalFunction open-input-string
"syn keyword lalFunction open-output-bytevector
"syn keyword lalFunction open-output-file
"syn keyword lalFunction open-output-string
"syn keyword lalFunction output-port-open?
"syn keyword lalFunction output-port?
"syn keyword lalFunction pair?
"syn keyword lalFunction peek-char
"syn keyword lalFunction peek-u8
"syn keyword lalFunction port?
"syn keyword lalFunction positive?
"syn keyword lalFunction push!
"syn keyword lalFunction pop!
"syn keyword lalFunction procedure?
"syn keyword lalFunction promise?
"syn keyword lalFunction quotient
"syn keyword lalFunction raise
"syn keyword lalFunction raise-continuable
"syn keyword lalFunction rational?
"syn keyword lalFunction rationalize
"syn keyword lalFunction read
"syn keyword lalFunction read-bytevector
"syn keyword lalFunction read-bytevector!
"syn keyword lalFunction read-char
"syn keyword lalFunction read-error?
"syn keyword lalFunction read-line
"syn keyword lalFunction read-string
"syn keyword lalFunction read-u8
"syn keyword lalFunction real-part
"syn keyword lalFunction real?
"syn keyword lalFunction remainder
"syn keyword lalFunction reverse
"syn keyword lalFunction round
"syn keyword lalFunction scheme-report-environment
"syn keyword lalFunction set-car!
"syn keyword lalFunction set-cdr!
"syn keyword lalFunction shift!
"syn keyword lalFunction sin
"syn keyword lalFunction sqrt
"syn keyword lalFunction square
"syn keyword lalFunction str
"syn keyword lalFunction str-join
"syn keyword lalFunction string
"syn keyword lalFunction string->keyword
"syn keyword lalFunction string->list
"syn keyword lalFunction string->number
"syn keyword lalFunction string->symbol
"syn keyword lalFunction string->utf8
"syn keyword lalFunction string->vector
"syn keyword lalFunction string-append
"syn keyword lalFunction string-ci<=?
"syn keyword lalFunction string-ci<?
"syn keyword lalFunction string-ci=?
"syn keyword lalFunction string-ci>=?
"syn keyword lalFunction string-ci>?
"syn keyword lalFunction string-copy
"syn keyword lalFunction string-copy!
"syn keyword lalFunction string-downcase
"syn keyword lalFunction string-fill!
"syn keyword lalFunction string-foldcase
"syn keyword lalFunction string-for-each
"syn keyword lalFunction string-length
"syn keyword lalFunction string-map
"syn keyword lalFunction string-ref
"syn keyword lalFunction string-set!
"syn keyword lalFunction string-upcase
"syn keyword lalFunction string<=?
"syn keyword lalFunction string<?
"syn keyword lalFunction string=?
"syn keyword lalFunction string>=?
"syn keyword lalFunction string>?
"syn keyword lalFunction string?
"syn keyword lalFunction substring
"syn keyword lalFunction symbol->string
"syn keyword lalFunction symbol=?
"syn keyword lalFunction symbol?
"syn keyword lalFunction syntax-error
"syn keyword lalFunction tan
"syn keyword lalFunction textual-port?
"syn keyword lalFunction transcript-off
"syn keyword lalFunction transcript-on
"syn keyword lalFunction truncate
"syn keyword lalFunction truncate-quotient
"syn keyword lalFunction truncate-remainder
"syn keyword lalFunction truncate/
"syn keyword lalFunction type
"syn keyword lalFunction u8-ready?
"syn keyword lalFunction utf8->string
"syn keyword lalFunction unshift!
"syn keyword lalFunction values
"syn keyword lalFunction vector
"syn keyword lalFunction vector->list
"syn keyword lalFunction vector->string
"syn keyword lalFunction vector-append
"syn keyword lalFunction vector-copy
"syn keyword lalFunction vector-copy!
"syn keyword lalFunction vector-fill!
"syn keyword lalFunction vector-for-each
"syn keyword lalFunction vector-length
"syn keyword lalFunction vector-map
"syn keyword lalFunction vector-ref
"syn keyword lalFunction vector-set!
"syn keyword lalFunction vector?
"syn keyword lalFunction with-exception-handler
"syn keyword lalFunction with-input-from-file
"syn keyword lalFunction with-output-to-file
"syn keyword lalFunction write
"syn keyword lalFunction writeln
"syn keyword lalFunction write-bytevector
"syn keyword lalFunction write-char
"syn keyword lalFunction write-shared
"syn keyword lalFunction write-simple
"syn keyword lalFunction write-string
"syn keyword lalFunction write-u8
"syn keyword lalFunction write-str
"syn keyword lalFunction zero?
"
"hi def link lalBoolean          Boolean
"hi def link lalCharacter        Character
"hi def link lalComment          Comment
"hi def link lalConstant         Constant
"hi def link lalEmptyList        Constant
"hi def link lalData             Delimiter
"hi def link lalDatumComment     Comment
"hi def link lalDatumCommentForm Comment
"hi def link lalDelimiter        Delimiter
"hi def link lalError            Error
"hi def link lalExtraSyntax      Underlined
"hi def link lalFunction         Function
"hi def link lalIdentifier       Normal
"hi def link lalKeyword          Constant
"hi def link lalLibrarySyntax    PreProc
"hi def link lalMultilineComment Comment
"hi def link lalNumber           Number
"hi def link lalParentheses      Normal
"hi def link lalQuasiquote       Delimiter
"hi def link lalQuote            Delimiter
"hi def link lalSpecialSyntax    Special
"hi def link lalString           String
"hi def link lalMultilineString  String
"hi def link lalSymbol           Normal
"hi def link lalSyntax           Statement
"hi def link lalSyntaxSyntax     PreProc
"hi def link lalTypeSyntax       Type
"hi def link lalTodo             Todo
"
"" sync on a line starting with a ( ... ) form
"syn sync match matchPlace grouphere NONE "^("
syn sync lines=500
""syn sync ccomment lalMultilineComment

let b:current_syntax = "wlambda"
