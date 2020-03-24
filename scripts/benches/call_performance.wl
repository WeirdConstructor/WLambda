# VM brings about 5% speed improvement
# time with vm: 1270ms, time with box closures: 1348
# on my linux machine
# weictr@lockwork:~/devel/rust/wlambda$ ./wlambda_pre_vm scripts/benches/call_performance.wl
# $[1355,10000000]
# $[1335,10000000]
# $[1339,10000000]
# $[1348,10000000]
# $[1346,10000000]
# $[1994,10000000]
# $[2000,10000000]
# $[2002,10000000]
# $[1988,10000000]
# $[1989,10000000]
# weictr@lockwork:~/devel/rust/wlambda$ wlambda scripts/benches/call_performance.wl
# $[1280,10000000]
# $[1279,10000000]
# $[1286,10000000]
# $[1273,10000000]
# $[1275,10000000]
# $[1866,10000000]
# $[1868,10000000]
# $[1870,10000000]
# $[1872,10000000]
# $[1874,10000000]


range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    range 1 10000000 1 {||
        .x = x + 1;
    };
    x
} };


range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !inc = { .x = x + 1};
    range 1 10000000 1 {|| inc[]; };
    x
} }
