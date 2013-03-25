import std.stdio;
import cstdio = std.c.stdio;
import std.string;
import std.algorithm;
import common;


/* Machine state */
struct AAM {
  /* Registers */
  short IR1;
  short IR2;
  short  IP;
  short  XP;
  short ACC;
  short MAR;
  short MDR;

  /* Condition Bits */
  bool   CF;
  bool   OF;
  bool   ZF;
  bool HALT;

  /* Main Memory */
  bool[MEM_SLOTS * SLOT_SIZE] RAW;
  short[MEM_SLOTS] MEM;
}


/* Pretty print a machine's registers to STDOUT. */
void regDump(AAM m) {
  writefln("IR1:    %016b 0x%04x", m.IR1, m.IR1);
  writefln("IR2:    %016b 0x%04x", m.IR2, m.IR2);
  writefln("IP:     %016b 0x%04x",  m.IP,  m.IP);
  writefln("XP:     %016b 0x%04x",  m.XP,  m.XP);
  writefln("ACC:    %016b 0x%04x", m.ACC, m.ACC);
  writefln("MAR:    %016b 0x%04x", m.MAR, m.MAR);
  writefln("MDR:    %016b 0x%04x", m.MDR, m.MDR);
  writefln("CF %s, OF %s, ZF %s, HALT %s",
    m.CF ?   "1" : "0",
    m.OF ?   "1" : "0",
    m.ZF ?   "1" : "0",
    m.HALT ? "1" : "0"
  );
}


/* Pretty print a machine's main memory to STDOUT. */
void memDump(AAM m) {
  for (int i = 0; i < MEM_SLOTS; i += 1) {
    short slot = m.MEM[i];
    writefln("0x%04x:  %016b", i * SLOT_SIZE, slot);
  }
}


/* RTL instructions implementation. */
AAM ipPlus1(AAM m) { m.IP += SLOT_SIZE; return m; }

AAM ipMinus1(AAM m) { m.IP -= SLOT_SIZE; return m; }

AAM notAccAndMdr(AAM m) { m.ACC = ~(m.ACC & m.MDR); return m; }

AAM notAccOrMdr(AAM m) { m.ACC = ~(m.ACC | m.MDR); return m; }

AAM notAcc(AAM m) { m.ACC = ~m.ACC; return m; }

AAM accAndMdr(AAM m) { m.ACC &= m.MDR; return m; }

AAM accNandMdr(AAM m) { m.ACC &= m.MDR; m.ACC = ~m.ACC; return m; }

AAM accXorMdr(AAM m) { m.ACC ^= m.MDR; return m; }

AAM accOrMdr(AAM m) { m.ACC |= m.MDR; return m; }

AAM accNorMdr(AAM m) { m.ACC |= m.MDR; m.ACC = ~m.ACC; return m; }

AAM accTimesMdr(AAM m) { m.ACC *= m.MDR; return m; }

AAM accPlusMdr(AAM m) { m.ACC += m.MDR; return m; }

AAM accMinusMdr(AAM m) { m.ACC -= m.MDR; return m; }

AAM accDivMdr(AAM m) { m.ACC /= m.MDR; return m; }

AAM accToMdr(AAM m) { m.MDR = m.ACC; return m; }

AAM ipToMar(AAM m) { m.MAR = m.IP; return m; }

AAM ipToXp(AAM m) { m.XP = m.IP; return m; }

AAM ir1ToMdr(AAM m) { m.MDR = m.IR1; return m; }

AAM ir2ToMdr(AAM m) { m.MDR = m.IR2; return m; }

AAM mdrToAcc(AAM m) { m.ACC = m.MDR; return m; }

AAM mdrToIp(AAM m) { m.IP = m.MDR; return m; }

AAM mdrToIr1(AAM m) { m.IR1 = m.MDR; return m; }

AAM mdrToIr2(AAM m) { m.IR2 = m.MDR; return m; }

AAM mdrToMar(AAM m) { m.MAR = m.MDR; return m; }

AAM xpToIp(AAM m) { m.IP = m.XP; return m; }

AAM unsetCf(AAM m) { m.CF = false; return m; }

AAM unsetOf(AAM m) { m.OF = false; return m; }

AAM unsetZf(AAM m) { m.ZF = false; return m; }

AAM setZf(AAM m) { m.ZF = true; return m; }

AAM setHalt(AAM m) { m.HALT = true; return m; }

AAM mdrToMemAtMar(AAM m) {
  assert(m.MAR/SLOT_SIZE < MEM_SLOTS);
  m.MEM[m.MAR/SLOT_SIZE] = m.MDR; return m;
}

AAM memAtMarToMdr(AAM m) {
  assert(m.MAR/SLOT_SIZE < MEM_SLOTS);
  m.MDR = m.MEM[m.MAR/SLOT_SIZE]; return m;
}



/* Read the binary file in the first argument,
   load it into a new machine, and execute. */
void main(string[] args) {
  auto f = File(args[1]);
  AAM thx1138;
  thx1138 = load(thx1138, f);
  thx1138 = run(thx1138);
}


/* Load a binary file into the machine. */
AAM load(AAM m, File f) {
  f.rawRead(m.RAW);
  for (int i = 0; i < MEM_SLOTS; i += 1) {
    short slot = 0;
    for (int j = 0; j < SLOT_SIZE; j += 1) {
      if (m.RAW[i*SLOT_SIZE + j]) {
        slot |= 1 << (SLOT_SIZE - j - 1);
      }
    }

    m.MEM[i] = slot;
  } return m;
}


/* Execute until HALT is reached. */
AAM run(AAM m) {
  while (!m.HALT) {
    m = fetch(m);
    m = execute(m);
  } return m;
}


/* Instruction fetch workflow, almost verbatim from CPU Sim. */
AAM fetch(AAM m) {
  m = unsetZf(m);
  if (m.ACC == 0) m = setZf(m);
  m = ipToMar(m);
  m = memAtMarToMdr(m);
  m = mdrToIr1(m);
  m = ipPlus1(m);
  m = ipToMar(m);
  m = memAtMarToMdr(m);
  m = mdrToIr2(m);
  m = ipPlus1(m);
  return m;
}


/* Execute a machine already loaded with instructions. */
AAM execute(AAM m) {
  string ir1 = format("%016b", m.IR1);
  string opcode = ir1[0..5];
  string instr  = lookupInstr(opcode);

  switch (instr) {
    case "nop": break;
    case "halt":
      m = setHalt(m);
      break;
    case "j":
      m = ir2ToMdr(m);
      m = mdrToIp(m);
      break;
    case "jz":
      m = ir2ToMdr(m);
      if (m.ACC == 0) m = mdrToIp(m);
      break;
    case "jltz":
      m = ir2ToMdr(m);
      if (m.ACC < 0) m = mdrToIp(m);
      break;
    case "jgtz":
      m = ir2ToMdr(m);
      if (m.ACC > 0) m = mdrToIp(m);
      break;
    case "add":
      m = ir2ToMdr(m);
      m = accPlusMdr(m);
      break;
    case "mul":
      m = ir2ToMdr(m);
      m = accTimesMdr(m);
      break;
    case "div":
      m = ir2ToMdr(m);
      m = accDivMdr(m);
      break;
    case "read":
      m.ACC = cast(short) cstdio.getchar();
      break;
    case "write":
      write(cast(char) m.ACC);
      stdout.flush();
      break;
    case "load":
      m = ir2ToMdr(m);
      m = mdrToMar(m);
      m = memAtMarToMdr(m);
      m = mdrToAcc(m);
      break;
    case "store":
      m = ir2ToMdr(m);
      m = mdrToMar(m);
      m = accToMdr(m);
      m = mdrToMemAtMar(m);
      break;
    case "not":
      m = notAcc(m);
      break;
    case "and":
      m = ir2ToMdr(m);
      m = accAndMdr(m);
      break;
    case "or":
      m = ir2ToMdr(m);
      m = accOrMdr(m);
      break;
    case "xor":
      m = ir2ToMdr(m);
      m = accXorMdr(m);
      break;
    case "nand":
      m = ir2ToMdr(m);
      m = accNandMdr(m);
      break;
    case "nor":
      m = ir2ToMdr(m);
      m = accNorMdr(m);
      break;
    case "andM":
      m = ir2ToMdr(m);
      m = mdrToMar(m);
      m = memAtMarToMdr(m);
      m = accAndMdr(m);
      break;
    case "orM":
      m = ir2ToMdr(m);
      m = mdrToMar(m);
      m = memAtMarToMdr(m);
      m = accOrMdr(m);
      break;
    case "addM":
      m = ir2ToMdr(m);
      m = mdrToMar(m);
      m = memAtMarToMdr(m);
      m = accPlusMdr(m);
      break;
    case "loadM":
      m = accToMdr(m);
      m = mdrToMar(m);
      m = memAtMarToMdr(m);
      m = mdrToAcc(m);
      break;
    case "call":
      m = ipToXp(m);
      m = ir2ToMdr(m);
      m = mdrToIp(m);
      break;
    case "ret":
      m = xpToIp(m);
      break;
    default: break;
  }

  return m;
}


unittest {
  /* Simple test: One "halt" instruction. */
  auto f = File("tmp", "a+");
  f.rawWrite(
    [false,false,false,false, true,false,false,false,
     false,false,false,false, false,false,false,false,
     false,false,false,false, false,false,false,false,
     false,false,false,false, false,false,false,false
    ]);
  f.rewind();

  /* Check layout in memory. */
  AAM m;
  m = load(m, f);
  assert(m.MEM[0] == 2048); // 0000 1000 0000 0000 => 2048
  for (auto i = 1; i < MEM_SLOTS; ++i)
    assert(m.MEM[i] == 0);

  /* Check registers. */
  assert(m.IP   == 0);
  assert(m.ACC  == 0);
  assert(m.ZF   == 0);
  assert(m.IR1  == 0);
  assert(m.IR2  == 0);
  assert(m.HALT == 0);

  /* Execute. Should halt immediately. */
  m = run(m);

  /* Re-check registers. */
  assert(m.IP   == 2 * SLOT_SIZE);
  assert(m.ACC  == 0);
  assert(m.ZF   == 1);
  assert(m.IR1  == 0b000100000000000);
  assert(m.IR2  == 0b000000000000000);
  assert(m.HALT == 1);

  /* Re-check layout in memory. */
  assert(m.MEM[0] == 2048); // 0000 1000 0000 0000 => 2048
  for (auto i = 1; i < MEM_SLOTS; ++i)
    assert(m.MEM[i] == 0);

  remove("tmp");
}