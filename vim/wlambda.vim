" Vim syntax file
" Language: WLambda
" Last Change: 2019-07-15
" Author: Weird Constructor <weirdconstructor@gmail.com>
" Maintainer: Weird Constructor <weirdconstructor@gmail.com>

"if exists("b:current_syntax")
"  finish
"endif

syntax clear

setlocal iskeyword=@,@-@,48-57,a-z,A-Z,$,:,?,_,-,+,%,<,>,192-255,`

syn match wlSymbol      ':[^[:space:]\.,;{}\[\]()~|="]\+'

syn match wlIdentifier  /[a-zA-Z_@?]\+[^[:space:]\.,;{}\[\]()~|=]*/
syn match wlAssignId    /[*a-zA-Z_@?]\+[^[:space:]\.,;{}\[\]()~|=]*/ contained
syn match wlMapKeyId    /[a-zA-Z_@?]\+[^[:space:]\.,;{}\[\]()~|=]*\s*\ze=/ contained

syn match wlComment /#.*$/ contains=wlTodo
syn keyword wlTodo TODO XXX FIXME   contained

syn keyword wlRepeat    iter jump ? if for while map filter range return break next match

syn keyword wlKeyword   panic not block unwrap unwrap_err error_to_str
syn keyword wlKeyword   sym is_some is_none is_err is_map is_vec is_fun is_str is_sym
syn keyword wlKeyword   is_ref is_wref is_bool is_bytes is_float is_int len type to_drop
syn keyword wlKeyword   float bool int on_error byte char is_byte is_char
syn keyword wlKeyword   _?

syn keyword wlKeyword std:append
syn keyword wlKeyword std:assert
syn keyword wlKeyword std:assert_eq
syn keyword wlKeyword std:bytes:from_hex
syn keyword wlKeyword std:bytes:from_vec
syn keyword wlKeyword std:bytes:to_hex
syn keyword wlKeyword std:bytes:to_vec
syn keyword wlKeyword std:chrono:timestamp
syn keyword wlKeyword std:cmp:num:asc
syn keyword wlKeyword std:cmp:num:desc
syn keyword wlKeyword std:cmp:str:asc
syn keyword wlKeyword std:cmp:str:desc
syn keyword wlKeyword std:copy
syn keyword wlKeyword std:deser:csv
syn keyword wlKeyword std:deser:json
syn keyword wlKeyword std:deser:msgpack
syn keyword wlKeyword std:displayln
syn keyword wlKeyword std:drop
syn keyword wlKeyword std:dump_func
syn keyword wlKeyword std:fold
syn keyword wlKeyword std:hash:fnv1a
syn keyword wlKeyword std:io:file:append
syn keyword wlKeyword std:io:file:read
syn keyword wlKeyword std:io:file:read_text
syn keyword wlKeyword std:io:file:write_safe
syn keyword wlKeyword std:io:flush
syn keyword wlKeyword std:io:read_some
syn keyword wlKeyword std:io:write
syn keyword wlKeyword std:io:write_some
syn keyword wlKeyword std:measure_time
syn keyword wlKeyword std:neg
syn keyword wlKeyword std:net:tcp:connect
syn keyword wlKeyword std:net:tcp:listen
syn keyword wlKeyword std:net:udp:new
syn keyword wlKeyword std:net:udp:recv
syn keyword wlKeyword std:net:udp:send
syn keyword wlKeyword std:num:abs
syn keyword wlKeyword std:num:int_to_closed_open01
syn keyword wlKeyword std:num:int_to_open01
syn keyword wlKeyword std:num:int_to_open_closed01
syn keyword wlKeyword std:pop
syn keyword wlKeyword std:prepend
syn keyword wlKeyword std:push
syn keyword wlKeyword std:rand
syn keyword wlKeyword std:rand:split_mix64_new
syn keyword wlKeyword std:rand:split_mix64_new_from
syn keyword wlKeyword std:rand:split_mix64_next
syn keyword wlKeyword std:rand:split_mix64_next_closed_open01
syn keyword wlKeyword std:rand:split_mix64_next_open01
syn keyword wlKeyword std:rand:split_mix64_next_open_closed01
syn keyword wlKeyword std:re:map
syn keyword wlKeyword std:re:match
syn keyword wlKeyword std:re:replace_all
syn keyword wlKeyword std:ref_id
syn keyword wlKeyword std:ser:csv
syn keyword wlKeyword std:ser:json
syn keyword wlKeyword std:ser:msgpack
syn keyword wlKeyword std:set_ref
syn keyword wlKeyword std:shuffle
syn keyword wlKeyword std:sort

syn keyword wlKeyword str
syn keyword wlKeyword std:str:cat
syn keyword wlKeyword std:str:from_char_vec
syn keyword wlKeyword std:str:from_latin1
syn keyword wlKeyword std:str:from_utf8
syn keyword wlKeyword std:str:from_utf8_lossy
syn keyword wlKeyword std:str:join
syn keyword wlKeyword std:str:len
syn keyword wlKeyword std:str:pad_end
syn keyword wlKeyword std:str:pad_start
syn keyword wlKeyword std:str:to_bytes
syn keyword wlKeyword std:str:to_bytes_latin1
syn keyword wlKeyword std:str:to_char_vec
syn keyword wlKeyword std:str:to_lowercase
syn keyword wlKeyword std:str:to_uppercase
syn keyword wlKeyword std:str:write
syn keyword wlKeyword std:strengthen
syn keyword wlKeyword std:take
syn keyword wlKeyword std:to_drop
syn keyword wlKeyword std:to_no_arity
syn keyword wlKeyword std:to_ref
syn keyword wlKeyword std:uneg
syn keyword wlKeyword std:unshift
syn keyword wlKeyword std:weaken
syn keyword wlKeyword std:wlambda:version
syn keyword wlKeyword std:write_str
syn keyword wlKeyword std:writeln
syn keyword wlKeyword std:yay

syn match wlValue     '\$s'
syn match wlValue     '\$self'
syn match wlValue     '\$\\'
syn match wlValue     '\$d'
syn match wlValue     '\$data'
syn match wlValue     '\$t'
syn match wlValue     '\$true'
syn match wlValue     '\$f'
syn match wlValue     '\$false'
syn match wlValue     '\$n'
syn match wlValue     '\$none'
syn match wlValue     '\$e'
syn match wlValue     '\$error'
syn match wlRefData   '$@i'
syn match wlRefData   '$@int'
syn match wlRefData   '$@f'
syn match wlRefData   '$@float'
syn match wlRefData   '$@s'
syn match wlRefData   '$@string'
syn match wlRefData   '$@b'
syn match wlRefData   '$@bytes'
syn match wlRefData   '$@v'
syn match wlRefData   '$@vec'
syn match wlRefData   '$@m'
syn match wlRefData   '$@map'
syn match wlRefData   '$@@'
syn match wlRefData   '$iter'
syn match wlRefData   '$+'
syn match wlRefData   '$&'
syn match wlRefData   '$&&'
syn match wlRefData   '$weak&'
syn match wlRefData   '$w&'
syn match wlRefData   '$\*'
syn match wlRefData   '$:'
syn match wlRefData   '=>'

syn match wlStructMatch '$P'
syn match wlCodeExpr    '$c'
syn match wlCodeExpr    '$code'

syn match wlValue     '[-+]\?[0-9a-fA-F.]\@<!\d\+'
syn match wlValue     '[-+]\?[0-9a-fA-F.]\@<!\d\+\.\d\+'
syn match wlValue     '[-+]\?0x[a-fA-F0-9]\+'
syn match wlValue     '[-+]\?0x[a-fA-F0-9]\+\(\.[a-fA-F0-9]\+\)\?'
syn match wlValue     '[-+]\?0b[01]\+'
syn match wlValue     '[-+]\?0b[01]\+\(\.[01]\+\)\?'
syn match wlValue     '[-+]\?0o[0-8]\+'
syn match wlValue     '[-+]\?0o[0-8]\+\(\.[0-8]\+\)\?'
syn match wlValue     '[-+]\?[0-9]\+r[0-9a-zA-Z]\+'
syn match wlValue     '[-+]\?[0-9]\+r[0-9a-zA-Z]\+\(\.[0-9a-zA-Z]\+\)\?'

syn match wlFuncCombinators '|'
syn match wlFuncCombinators '||'
syn match wlFuncCombinators '|\?[>~]'
syn match wlFuncCombinators '&>'
syn match wlFuncCombinators '<&'

" WLambda Regex Patterns

syn region wlRxPattern     matchgroup=wlString start="\$r\z(.\)"       matchgroup=wlRxPattern end="\z1"   keepend contains=@wlPattern
syn region wlRxPattern     matchgroup=wlString start="\$r\["           matchgroup=wlRxPattern end="\]"    keepend contains=@wlPattern
syn region wlRxPattern     matchgroup=wlString start="\$r("            matchgroup=wlRxPattern end=")"     keepend contains=@wlPattern
syn region wlRxPattern     matchgroup=wlString start="\$r{"            matchgroup=wlRxPattern end="}"     keepend contains=@wlPattern
syn region wlRxPattern     matchgroup=wlString start="\$r<"            matchgroup=wlRxPattern end=">"     keepend contains=@wlPattern

syn cluster wlPattern contains=wlPatternFun,wlPatternRx,wlPatternRxAtom,wlPatternRxErr,wlStringSpec,wlPatternRxEsc,wlPatternGroup

syn match wlPatternRxAtom '[^()\[\]{}!?\\|^,'&:;$*=/]'          contained
syn match wlPatternRxErr  '[{}?\\|^,'&:;$*=]'                   contained
syn match wlPatternRxEsc  '\\.'                                 contained
syn match wlPatternFun    '|'                                   contained
syn match wlPatternFun    '\*'                                  contained
syn match wlPatternFun    '?'                                   contained
syn match wlPatternRx     '\$[*+?!=^$sS]'                       contained
syn match wlPatternRx     '\$<[*+?]'                            contained
syn match wlPatternRx     '\$&[UL]'                             contained

syn region wlPatternGroup matchgroup=wlPatternGroup start='('   skip=+\\\\\|\\)+ end=')' contained keepend contains=@wlPattern
syn region wlPatternGroup matchgroup=wlPatternGroup start='(\^' skip=+\\\\\|\\)+ end=')' contained keepend contains=@wlPattern
syn region wlPatternGroup matchgroup=wlPatternGroup start='\['   skip=+\\\\\|\\]+  end='\]' contained keepend contains=@wlPattern
syn region wlPatternGroup matchgroup=wlPatternGroup start='\[\^' skip=+\\\\\|\\]+  end='\]' contained keepend contains=@wlPattern

" WLambda Selectors

syn region wlSelector    matchgroup=wlString start="\$S\z(.\)" end="\z1"re=s-1 keepend contains=@wlSelPath
syn region wlSelector    matchgroup=wlString start="\$S\["     end="\]"  keepend contains=@wlSelPath
syn region wlSelector    matchgroup=wlString start="\$S("      end=")"   keepend contains=@wlSelPath
syn region wlSelector    matchgroup=wlString start="\$S{"      end="}"   keepend contains=@wlSelPath
syn region wlSelector    matchgroup=wlString start="\$S<"      end=">"   keepend contains=@wlSelPath

syn cluster wlSelPath contains=wlSelPathSep,@wlPattern,wlSelNodeMatch

syn match wlSelPathSep  '/'         contained
syn match wlSelPathSep  '\^'        contained
syn match wlSelPathSep  '\*\*'      contained
syn match wlSelPathSep  '\*'        contained
syn match wlSelPathSep  '&'         contained
syn match wlSelPathSep  '|'         contained

syn match wlSelNodeMatch ':'        contained nextgroup=wlSelNodeNeg,wlSelNodeType,wlSelNodeSubSel
syn match wlSelNodeNeg   '!'        contained nextgroup=wlSelNodeType,wlSelNodeSubSel

syn match wlSelNodeType  'type\s*=' contained nextgroup=wlPattern
syn match wlSelNodeType  'str\s*='  contained nextgroup=wlPattern

syn region wlSelNodeSubSel matchgroup=wlPatternGroup start='(' end=')' contained keepend contains=@wlSelPath
syn region wlSelNodeSubSel matchgroup=wlPatternGroup start='{' end='}' contained keepend contains=@wlPattern,wlSelNodeKVAssign

syn match wlSelNodeKVAssign  '='  contained
syn match wlSelNodeKVAssign  ','  contained

" WLambds String Escapes

syn match wlStringSpec  /\\x[a-f0-9A-F][a-f0-9A-F]/  contained
syn match wlStringSpec  /\\["'\\nrt0]/               contained
syn match wlStringSpec  /\\u{[a-f0-9A-F]\+}/         contained

" WLambda Strings

syn region wlString     start="\$[Qq]\z(.\)" end="\z1"
syn region wlString     start="\$[Qq]\["     end="\]"
syn region wlString     start="\$[Qq]("      end=")"
syn region wlString     start="\$[Qq]{"      end="}"
syn region wlString     start="\$[Qq]<"      end=">"

syn region wlQSymbol    start=+:"+      skip=+\\\\\|\\"+  end=+"+ contains=wlStringSpec
syn region wlString     start=+"+       skip=+\\\\\|\\"+  end=+"+ contains=wlStringSpec
syn region wlString     start=+\$b"+    skip=+\\\\\|\\"+  end=+"+ contains=wlStringSpec
syn region wlString     start=+'+       skip=+\\\\\|\\'+  end=+'+ contains=wlStringSpec
syn region wlString     start=+\$b'+    skip=+\\\\\|\\'+  end=+'+ contains=wlStringSpec

" WLambda number vectors:
syn region wlNumVec matchgroup=wlRefData start=+$[poif](+ end=+)+ contains=@wlExpr

syn match wlAssign   '\(^\|\s\+\|{\|;\|\\\)\@<=\.' nextgroup=wlAssignId,wlDestr
syn match wlDefine   '!'                       nextgroup=wlImport,wlExport,wlDefTag,wlAssignId,wlDestr
syn match wlDefTag   ':global' skipwhite skipnl nextgroup=wlAssignId contained
syn match wlImport   '@import' skipwhite skipnl nextgroup=wlAssignId contained
syn match wlImport   '@wlambda' skipwhite skipnl contained
syn match wlExport   '@export' skipwhite skipnl nextgroup=wlAssignId contained
syn region wlDestr         matchgroup=wlDestrDelim start="(" end=")" transparent contained contains=wlIdentifier,wlComment
syn match wlArity     '|\s*\d\+\s*<\s*\d\+\s*|' contained
syn match wlArity     '|\s*\d\+\s*|'            contained
syn match wlArity     '|\s*|'                   contained

syn match wlSplice      '*'           contained nextgroup=@wlExpr

syn cluster wlExpr  contains=wlComment,wlRefData,wlValue,wlIdentifier,wlKeyword,wlKeyword2,wlRepeat,wlSymbol,wlQSymbol,wlString,wlArgList,wlFunc,wlFuncCombinators,wlList,wlMap,wlNumVec,wlStructMatch,wlQuote,wlShortFunc,wlSelector,wlRxPattern

syn region wlMap       matchgroup=wlRefData start="\${" end="}"     transparent contains=@wlExpr,wlMapKeyId,wlSplice
syn region wlList      matchgroup=wlRefData start="\$\[" end="\]"   transparent contains=@wlExpr,wlSplice
syn region wlQuote     matchgroup=wlExprQuote start="(" end=")"     transparent contains=@wlExpr
syn region wlArgList   matchgroup=wlFuncDelims start="\[" end="\]"  transparent contains=@wlExpr
syn region wlFunc      matchgroup=wlFuncDelims start="{" end="}"    transparent contains=@wlExpr,wlAssign,wlDefine,wlArity
syn match wlShortFunc  '\\'                                                    nextgroup=@wlExpr,wlAssign,wlDefine,wlArity

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

hi def link wlSelector          String
hi def link wlSelPathSep        Function
hi def link wlSelNodeSubSel     Function
hi def link wlSelNodeKVAssign   Function
hi def link wlSelNodeNeg        Structure
hi def link wlSelNodeMatch      Structure
hi def link wlSelNodeType       Special

hi def link wlRxPattern     String
hi def link wlPatternFun    Function
hi def link wlPatternGroup  Function
hi def link wlPatternRx     Structure
hi def link wlPatternRxAtom String
hi def link wlPatternRxEsc  Special
hi def link wlPatternRxErr  Error

hi def link wlError         PreProc
hi def link wlImport        PreProc
hi def link wlExport        PreProc
hi def link wlString        String
hi def link wlStringSpec    Special
hi def link wlSymbol        Special
hi def link wlQSymbol       Special
hi def link wlStructMatch   Special
hi def link wlCodeExpr      Special
hi def link wlAssignId      Special
hi def link wlMapKeyId      Special
hi def link wlSplice        Function
hi def link wlRefData       Structure
hi def link wlDefTag        Type
hi def link wlArity         Type
hi def link wlSymbolSpec    Constant
hi def link wlIdentifier    Normal
hi def link wlComment       Comment
hi def link wlTodo          Todo

"" sync on a line starting with a ( ... ) form
"syn sync match matchPlace grouphere NONE "^("
syn sync lines=500
""syn sync ccomment lalMultilineComment

let b:current_syntax = "wlambda"
