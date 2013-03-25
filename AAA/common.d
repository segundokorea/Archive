module common;

import std.bitmanip;

const int MEM_SLOTS  = 512;
const int BYTE_SIZE  =   8;
const int SLOT_SIZE  =  16;
const int INSTR_SIZE =  32;

struct Slot {
  mixin(bitfields!(
    short, "opcode",  5,
    short,    "aux", 11,
    short,   "data", 16
  ));
}

short lookupOpcode(string name) {
  short[string] opcodes = [
    "nop":   0x00,
    "halt":  0x01,
    "j":     0x02,
    "jz":    0x05,
    "jltz":  0x06,
    "jgtz":  0x07,
    "add":   0x08,
    "mul":   0x09,
    "div":   0x0A,
    "read":  0x0B,
    "write": 0x0C,
    "load":  0x0D,
    "store": 0x0E,
    "not":   0x0F,
    "and":   0x10,
    "or":    0x11,
    "xor":   0x12,
    "nand":  0x13,
    "nor":   0x14,
    "andM":  0x15,
    "orM":   0x16,
    "addM":  0x17,
    "loadM": 0x1D,
    "call":  0x1E,
    "ret":   0x1F
  ];

  return opcodes[name];
}


string lookupInstr(string opcode) {
  string[string] instrs = [
    "00000": "nop",
    "00001": "halt",
    "00010": "j",
    "00101": "jz",
    "00110": "jltz",
    "00111": "jgtz",
    "01000": "add",
    "01001": "mul",
    "01010": "div",
    "01011": "read",
    "01100": "write",
    "01101": "load",
    "01110": "store",
    "01111": "not",
    "10000": "and",
    "10001": "or",
    "10010": "xor",
    "10011": "nand",
    "10100": "nor",
    "10101": "andM",
    "10110": "orM",
    "10111": "addM",
    "11101": "loadM",
    "11110": "call",
    "11111": "ret"
  ];

  return instrs[opcode];
}
