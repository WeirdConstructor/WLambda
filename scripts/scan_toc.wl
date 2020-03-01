!prel = std:io:file:read_text "src/prelude.rs";

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
    feed = \:feed{!(depth_marker, title) = @;
        ((len depth_marker) == 1) {
            return :feed $n;
        };

        ((len depth_marker) - 1 > $data.depth) {
            std:push $data.stack $data.current_count - 1;
            $data.current_count = 1;
            $data.depth = $data.depth + 1;
        };
        while { (len depth_marker) - 1 < $data.depth } {
            $data.current_count = std:pop $data.stack;
            $data.depth = $data.depth - 1;
            $data.current_count = $data.current_count + 1;
        };

        std:push $data.toc $[
            depth_marker,
            std:str:join "." $[*$data.stack, $data.current_count],
            title
        ];

        $data.current_count = $data.current_count + 1;
    },
};

!c = collector.new[];

!orig = prel;
# remove code fragments:
.prel = prel | std:re:replace_all $q_(?s)```.*?```_ {|| "" };

#!toc

#!root = section.new[];

#!section_counter
std:re:map $q_(#+)\s*(?:[1-9]+(?:\.[0-9])*)?\s+(.*)_ {
    std:displayln _.1 _.2;
    c.feed _.1 _.2;
} prel;

c._data.toc std:displayln;
