range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = $i(1,2);
    !y = $i(1,1);
    iter i $i(0, 10000000) {
        .x = x + y;
    };
    x
} };

range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = $i(1,2,3,4);
    !y = $i(1,1,1,1);
    iter i $i(0, 10000000) {
        .x = x + y;
    };
    x
} };

range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = $i(1,2,3,4);
    iter i $i(0, 10000000) {
        .x = x + $i(1,1,1,1);
    };
    x
} };
