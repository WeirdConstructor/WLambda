std:fs:copy "doc/wlambda_reference.md" "doc/wlambda_reference.bak";
!ref_doc_md = std:io:file:read_text "doc/wlambda_reference.md";

!sections = $[];

!cur_section = $none;
!cur_section_lines = $[];

iter line (ref_doc_md "\n" => 0) {
    if line &> $r/$^(^$+#)*(<a*<?a>)$*$s(^*)$$/ {
        if cur_section &> is_some {
            # std:displayln ">>> " cur_section;
            std:push sections $[cur_section, cur_section_lines];
            # iter l cur_section_lines {
            #     std:displayln " : " l;
            # };
        };

        .cur_section_lines = $[];
        .cur_section =
            std:str:replace "\\_" "_" $\.2;
    } {
        std:push cur_section_lines line;
    };
};

!documentation = std:ser:json sections;
std:io:file:write_safe "src/cmdline_doc.json" documentation;
