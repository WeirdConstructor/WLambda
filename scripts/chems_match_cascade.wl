!get_k = {!(start) = @;
    $@m (std:deser:json ~ std:io:file:read_text "chemical_elements.json") {
        if len[_.symbol] == 1 &or (0 => 1)[_.symbol] != start { return $n; };
        $+ _.symbol _
    }
};
!first2 = $@m (std:deser:json ~ std:io:file:read_text "chemical_elements.json") {
    if len[_.symbol] == 1 { return $n; };
    $+ (0 => 1 _.symbol) _
};
!first1 = $@m (std:deser:json ~ std:io:file:read_text "chemical_elements.json") {
    if len[_.symbol] == 2 { return $n; };
    $+ (0 => 1 _.symbol) _
};
!ooo = {!S = _;
    !s = "";
    .s +>= "\n    match s.at(1) {\n";
    iter k (get_k S) {
        .s +>= (
            $F"        '{}' => Some({}), // {}: {}\n"
                (1 => 1 k.1)
                k.0.atomic_number k.0.symbol k.0.name
        );
    };
    .s +>= "        _ => None,\n    },";
    s
};
!fkeys = std:keys first2;
std:sort fkeys;
iter fk fkeys {
    !mm = ooo fk;
    std:displayln ~ $F"'{}' | '{}' => {}" fk std:str:to_lowercase[fk] mm;
};
!f1keys = std:keys first1;
std:sort f1keys;
iter k f1keys {
    !k = $p(first1.(k), k);
    std:displayln ~
        $F"'{}' | '{}' => Some({}), // {}: {}"
            k.1
            std:str:to_lowercase[k.1]
            k.0.atomic_number
            k.0.symbol
            k.0.name
};
