## [Chip-8 (Ruby Quiz #88)](http://www.rubyquiz.com/quiz88.html)

Extremely limited interpreter for the CHIP-8 language. Pass in a program to `emul` to evaluate the program, print a debuggable version, and dump the register contents like so:

    $ ./emul test.c8
    => 0x77
    => 0x45
    => 0x78
    => 0x45
    => 0x7D
    => 0x45
    => 0x00
    => 0x8A
    => 0xBB
    => 0x45
    => 0x76
    => 0xEC
    => 0xFF
    => 0x11
    => 0x0F
    000 - 6177 :: V1  = 119
    001 - 6245 :: V2  = 69
    002 - 7101 :: V1 += 1
    003 - 8320 :: V3  = V2
    004 - 8121 :: V1 |= V2
    005 - 8122 :: V1 &= V2
    006 - 8233 :: V2 ^= V3
    007 - 8134 :: V1 += V3
    008 - 8235 :: V2 -= V3
    009 - 8106 :: V1 >> 1
    00A - 8327 :: V3  = V2 - V3
    00B - 830E :: V3 << 1
    00C - 64FF :: V4  = 255
    00D - C411 :: V4  = RAND & 17
    00E - 32BB :: Skip if V2 == 187
    00F - 1000 :: Jump to 0
    010 - 0000 :: EOF
    #<Chip8:0x007feceb0becf0>
    00000000
    01000101
    10111011
    11101100
    00010001
    00000000
    00000000
    00000000
    00000000
    00000000
    00000000
    00000000
    00000000
    00000000
    00000000
    00000000
