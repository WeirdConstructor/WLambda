std:io:lines {
    !line = std:str:trim _;
    unwrap ~ std:re:match $q;\((r?)'([^']+)',\s*(r?)'([^']+)',\s*([^,\)]+)(?:,\s*[^,]*,\s*(r?)'(.*)')?\); line {
        !r = ? _.5 == "SUCCEED" { _.7 } { "-nomatch-" };
        std:displayln ~ std:str:cat
            "assert_eq!(rep("
            std:str:pad_end[30, " ", std:str:cat _.1 "\"" _.2 "\","]
            std:str:pad_end[30, " ", std:str:cat _.3 "\"" _.4 "\"),"]
            _.6 "\"" r "\");";
    };
};
