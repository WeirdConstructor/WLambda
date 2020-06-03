std:io:file:copy "doc/wlambda_reference.md" "doc/wlambda_reference.bak";
!ref_doc_md = std:io:file:read_text "doc/wlambda_reference.md";

!make_new_section_str = { std:str:cat _.1 " - " _.2 };

!escape_underscore = {
    std:re:replace_all $q/([a-zA-Z0-9])_([a-zA-Z0-9])/ { _.1 "\\_" _.2 } ~
        (std:str:replace "\\_" "_" _)
};

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
    feed_section_header = \:feed{!(depth_marker, title, line) = @;
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

!orig = ref_doc_md;
# remove code fragments, or else we get all the # comments
# from the WLambda code examples:
.ref_doc_md = ref_doc_md | std:re:replace_all $q_(?s)```.*?```_ {|| "" };

std:re:map $q_(#+)\s*(?:<a name=.*?</a>\s*)?(?:[1-9][0-9]*(?:\.[0-9]+)*\s*)?\s*-\s+(.*)_ {
    c.feed_section_header _.1 _.2 _.0;
} ref_doc_md;

# Replace section headers by anchored section headers:
c._data.toc {
    .orig = orig
        | std:str:replace _.3
              ~ std:str:cat _.0 " <a name=\"" _.4 "\"></a>"
              ~ escape_underscore[make_new_section_str[_]];
};

# Create new TOC:
!new_toc = std:str:join "\n" ~ $@v c._data.toc {
    !pad = "";
    range 3 (len _.0) 1 {|| .pad = pad "  "; };
    $+ ~
        std:str:cat
            pad "- [" _.1 "](#" _.4 ") "
            escape_underscore[_.2];
};

.orig = orig | std:re:replace_all $q_(?s)\*\*Table Of Contents:\*\*.*?-----_ {||
    std:str:cat "**Table Of Contents:**\n\n" new_toc "\n\n-----"
};

# Checking internal links:
std:re:map $q_\[[^\]]+\]\((#\d+-[^\)]+)\)_ {
    !found = $@i iter t c._data.toc {
        ? _.1 == ("#" t.4)
            \$+ 1;
    };
    ? not[found] {
        std:displayln "Link not found: " _.0;
    };
} orig;

std:io:file:write_safe "doc/wlambda_reference.md" orig;

# Now load prelude.rs and replace the REFERENCE DOC START  ... END with
# the generated documentation in the variable `orig`:

std:io:file:copy "src/prelude.rs" "src/prelude.bak";
!prelude_rs = std:io:file:read_text "src/prelude.rs";

.prelude_rs = prelude_rs | std:re:replace_all $q_(?s)\[\]: ---- REFERENCE DOC START ----.*?\[\]: ---- REFERENCE DOC END ----_ {||
    std:str:cat "[]: ---- REFERENCE DOC START ----\n\n" orig "\n\n[]: ---- REFERENCE DOC END ----"
};

std:io:file:write_safe "src/prelude.rs" prelude_rs;
