std:displayln "assign $p(a, b)";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !p = 0;
    !i = $p(10, 20);
    while x < 10000000 {
        .p = i;
        .p = i;
        .p = i;
        .p = i;
        .x = x + 1;
    };
    x
} };

std:displayln "new $p(a, b)";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !p = 0;
    while x < 10000000 {
        .p = $p(10, 20);
        .p = $p(10, 20);
        .p = $p(10, 20);
        .p = $p(10, 20);
        .x = x + 1;
    };
    x
} };

std:displayln "assign $o(...)";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !p = 0;
    !i = $o($p(10, 20));
    while x < 10000000 {
        .p = i;
        .p = i;
        .p = i;
        .p = i;
        .x = x + 1;
    };
    x
} };

std:displayln "new $o(...)";
range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !p = 0;
    while x < 10000000 {
        .p = $o(10);
        .p = $o(10);
        .p = $o(10);
        .p = $o(10);
        .x = x + 1;
    };
    x
} };
