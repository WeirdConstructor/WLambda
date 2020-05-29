std:io:file:copy "src/prelude.rs" "src/prelude.bak";
!prel = std:io:file:read_text "src/prelude.rs";

!make_new_section_str = { std:str:cat _.1 " - " _.2 };

!strip_for_anchor_name = {
    std:str:to_lowercase
        ~ std:re:replace_all $q$ $ {|| "-" }
        ~ std:re:replace_all $q$[^ a-zA-Z0-9-]$ {|| "" } _
};

!collector = ${
    new = \${
        _proto = $self,
        _data  = ${
            toc           = $[],
            current_count = 1,
            depth         = 1,
            stack         = $[],
        }
    },
    feed = \:feed{!(depth_marker, title, line) = @;
        ((len depth_marker) == 1) {
            return :feed $n;
        };

        while { (len depth_marker) - 1 > $data.depth } {
            std:push $data.stack $data.current_count - 1;
            $data.current_count = 1;
            $data.depth = $data.depth + 1;
        };
        while { (len depth_marker) - 1 < $data.depth } {
            $data.current_count = std:pop $data.stack;
            $data.depth = $data.depth - 1;
            $data.current_count = $data.current_count + 1;
        };

        !section_number =
            std:str:join "." $[*$data.stack, $data.current_count];

        std:push $data.toc $[
            depth_marker,
            section_number,
            title,
            line,
            strip_for_anchor_name ~ std:str:cat section_number " " title,
        ];

        $data.current_count = $data.current_count + 1;
    },
};

!c = collector.new[];

!orig = prel;
# remove code fragments, or else we get all the # comments
# from the WLambda code examples:
.prel = prel | std:re:replace_all $q_(?s)```.*?```_ {|| "" };

std:re:map $q_(#+)\s*(?:<a name=.*?</a>\s*)?(?:[1-9][0-9]*(?:\.[0-9]+)*\s*)?\s*-\s+(.*)_ {
    c.feed _.1 _.2 _.0;
} prel;

c._data.toc {
    .orig = orig
        | std:str:replace _.3
              ~ std:str:cat _.0 " <a name=\"" _.4 "\"></a>"
              ~ make_new_section_str[_];
};

!new_toc = std:str:join "\n" ~ $@v c._data.toc {
    !pad = "";
    range 3 (len _.0) 1 {|| .pad = pad "  "; };
    $+ ~ std:str:cat pad "- [" _.1 "](#" _.4 ") " _.2
};

.orig = orig | std:re:replace_all $q_(?s)\*\*Table Of Contents:\*\*.*?-----_ {||
    std:str:cat "**Table Of Contents:**\n\n" new_toc "\n\n-----"
};
#std:io:stdout:print orig;

std:io:file:write_safe "src/prelude.rs" orig;

#std:io:stdout:print new_toc;
