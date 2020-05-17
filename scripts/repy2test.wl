std:io:lines {
    !line = std:str:trim _;
    unwrap ~
        std:re:match $q{\(r?'([^']+)',\s*'([^']+)',\s*([^,\)]+)(?:,\s*[^,]*,\s*'(.*)')?\)} line {
#            std:displayln line "=>" @;
            !r = ? _.3 == "SUCCEED" { _.4 } { "-nomatch-" };
            std:displayln ~ std:str:cat
                "assert_eq!(rep("
                std:str:pad_end[30, " ", std:str:cat "r#\"" _.1 "\"#,"]
                std:str:pad_end[30, " ", std:str:cat "r#\"" _.2 "\"#),"]
                "r#\"" r "\"#);";
        };
};
