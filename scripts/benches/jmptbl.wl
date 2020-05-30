std:displayln "? tbl";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !r = 0;
    while x < 1000000 {
        .r = r + (? x % 2 == 0 3 1);
        .x = x + 1;
    };
    r
} };

std:displayln "jump";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !r = 0;
    while x < 1000000 {
        .r = r + (jump x % 2 3 1);
        .x = x + 1;
    };
    r
} };

std:displayln "match";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !r = 0;
    while x < 1000000 {
        .r = r + (match x % 2 1 => 3 0 => 1);
        .x = x + 1;
    };
    r
} };

std:displayln "match2";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !r = 0;
    while x < 1000000 {
        .r = r + (match x % 5
                    4 => 3
                    3 => 3
                    1 => 3
                    0 => 1);
        .x = x + 1;
    };
    r
} };

std:displayln "match3";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !r = 0;
    while x < 1000000 {
        .r = r + (match x % 5 x => $\.x);
        .x = x + 1;
    };
    r
} };
