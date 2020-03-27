# VM brings about 19-34% speed improvement
# time with vm: 1270ms, time with box closures: 1348
# on my linux machine

# weictr@lockwork:~/devel/rust/wlambda$ wlambda_no_nvec scripts/benches/call_performance.wl
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

# weictr@lockwork:~/devel/rust/wlambda$ ./wlambda_pre_vm scripts/benches/call_performance.wl
#832040
#time: $[2856,832040]
#832040
#time: $[2857,832040]
#832040
#time: $[2855,832040]
#832040
#time: $[2858,832040]
#832040
#time: $[2853,832040]
#$[1406,10000000]
#$[1407,10000000]
#$[1406,10000000]
#$[1404,10000000]
#$[1399,10000000]
#$[2026,10000000]
#$[2045,10000000]
#$[2028,10000000]
#$[2026,10000000]
#$[2032,10000000]
#
#weictr@lockwork:~/devel/rust/wlambda$ wlambda scripts/benches/call_performance.wl 
#832040
#time: $[3315,832040]
#832040
#time: $[3298,832040]
#832040
#time: $[3298,832040]
#832040
#time: $[3307,832040]
#832040
#time: $[3303,832040]
#$[908,10000000]
#$[898,10000000]
#$[900,10000000]
#$[901,10000000]
#$[899,10000000]
#$[1625,10000000]
#$[1629,10000000]
#$[1643,10000000]
#$[1624,10000000]
#$[1666,10000000]

range 1 5 1 {||
    std:displayln "time:" ~ std:measure_time :ms {
        !fib = $n;
        .fib = \:fib {!n = _;
            match n
                0 {|| return :fib 0 }
                1 {|| return :fib 1 }
                {|| return :fib (fib n - 1) + (fib n - 2); };
        };
        std:displayln ~ fib 30;
    };
};

range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    range 1 10000000 1 {||
        .x = x + 1;
    };
    x
} };


range 1 5 1 {|| std:displayln ~ std:measure_time :ms {||
    !x = 0;
    !inc = { .x = x + 1 };
    range 1 10000000 1 {|| inc[]; };
    x
} }
