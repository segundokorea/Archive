import std.stdio;
import std.conv;
import std.math;
import std.file;
import std.array;
import std.format;
import std.string;
import std.bitmanip;
import std.algorithm;
import pegged.pegged.grammar;
import common;


/* Using the PEGGED parser generator for D.
   See https://github.com/PhilippeSigaud/Pegged. */
mixin(grammar(`
Assembly:
  Program <- :WS* ((:Comment / Expr) :WS*)*
  Expr    <- Macro / Instr / Label
  Comment <- :"#" :space? (!EOL .)* :EOL
  Label   <- identifier :":"
  Instr   <- identifier (:" "+ word)? :EOL
  Macro   <- :"." identifier :space (!EOL .)* :EOL
  word    <- [\-a-zA-Z0-9]+
  space   <- " " / "\t" / "\r" / "\n"
  WS      <- space / endOfLine
  EOL     <- endOfLine / endOfInput
`));


/* Sanity test for generated parser. */
unittest {
  auto testProgram = `
    zero:  .byte 0
    start: halt
  `;
  auto ptree = Assembly(testProgram);

  /* Parsing and program. */
  assert(ptree.name == "Assembly");
  assert(ptree.children.length == 1);
  auto program = ptree.children[0];
  assert(program.name == "Assembly.Program");
  assert(program.children.length == 4);

  /* Expr 1: label "zero" */
  auto labelZero = program.children[0];
  assert(labelZero.name == "Assembly.Expr");
  assert(labelZero.children.length == 1);
  labelZero = labelZero.children[0];
  assert(labelZero.name == "Assembly.Label");

  /* Expr 2: macro "byte" */
  auto macroByte = program.children[1];
  assert(macroByte.name == "Assembly.Expr");
  assert(macroByte.children.length == 1);
  macroByte = macroByte.children[0];
  assert(macroByte.name == "Assembly.Macro");

  /* Expr 3: label "start" */
  auto labelStart = program.children[2];
  assert(labelStart.name == "Assembly.Expr");
  assert(labelStart.children.length == 1);
  labelStart = labelStart.children[0];
  assert(labelStart.name == "Assembly.Label");

  /* Expr 4: instr "halt" */
  auto instrHalt = program.children[3];
  assert(instrHalt.name == "Assembly.Expr");
  assert(instrHalt.children.length == 1);
  instrHalt = instrHalt.children[0];
  assert(instrHalt.name == "Assembly.Instr");
}


/* Take the assembly program file supplied in
 * the first argument and compile the contents
 * to an AAS-ready binary printed to STDOUT.
 */
void main(string[] args) {
  string source  = cast(string) read(args[1]);
  auto parseTree = Assembly(source);
  auto exprs     = Exprs(parseTree);
  Binary(exprs, stdout);
}


/* Expressions and expression types. */
enum AType : short { ALabel, AInstr, AMacro, AError }

struct AExpr {
  short       type; // What is this thing?
  int       offset; // Where is this thing?
  int       length; // How big is this thing?
  string      name; // Really what is thing?
  string[] rawArgs; // Load with raw arguments
  int[]       args; // Load with processed arguments
  string     ascii; // Reserved for ASCII directive
}


/* Find the real type of a parsed expression. */
short exprType(ParseTree expr) {
  switch (expr.name) {
    case "Assembly.Label": return AType.ALabel;
    case "Assembly.Instr": return AType.AInstr;
    case "Assembly.Macro": return AType.AMacro;
    default: return AType.AError;
  }
}


/* Convert a parse tree into a list of expressions
   to be executed on our machine. */
AExpr[] Exprs(ParseTree p) {
  assert(p.name == "Assembly");
  auto program = p.children[0];
  assert(program.name == "Assembly.Program");


  /* Pass #1: Get initial instructions and macros
              with offsets and raw args */
  AExpr[] exprs;
  AExpr[] labels;
  int offset = 0;
  foreach (ref child; program.children) {
    assert(child.name == "Assembly.Expr");
    auto expr = child.children[0];

    AExpr aExpr;
    aExpr.type    = exprType(expr);
    aExpr.offset  = offset;
    aExpr.name    = expr.matches[0];
    aExpr.rawArgs = expr.matches[1 .. expr.matches.length];

    switch(aExpr.type) {
      case AType.ALabel:
        labels ~= [aExpr];
        break;
      case AType.AInstr:
        if (expr.children.length > 0) {
          auto arg = expr.children[0];
          assert(arg.name == "Assembly.word" || arg.name == "Assembly.identifier");
          aExpr.rawArgs = [join(arg.matches)];
        }
        aExpr.length = INSTR_SIZE;
        offset += INSTR_SIZE;
        exprs ~= [aExpr];
        break;
      case AType.AMacro:
        int length = 0;
        switch (aExpr.name) {
          case "byte":
            string arg = join(aExpr.rawArgs, "");
            aExpr.args.length = 1;
            aExpr.args[0] = parse!int(arg);
            length     = SLOT_SIZE;
            break;
          case "ascii":
            string data = join(aExpr.rawArgs, "");
                   data = data[1 .. data.length - 1];
            int nBytes  = cast(int) data.length;
            length      = SLOT_SIZE * nBytes;
            aExpr.ascii = data;
            break;
          default:
        }
        aExpr.length = length;
        offset += length;
        exprs  ~= [aExpr];
        break;
      default:
    }
  }


  /* Pass #2: Replace labels with their offsets. */
  foreach (ref expr; exprs) {
    if (expr.name != "byte") expr.args.length = expr.rawArgs.length;

    for (int i = 0; i < expr.rawArgs.length; i += 1) {
      string rawArg = expr.rawArgs[i];
      int arg;

      try {
        arg = parse!int(rawArg);
      } catch (Exception e) {
        foreach (ref label; labels) {
          if (label.name == rawArg) {
            arg = label.offset;
            break;
          }
        }
      }

      if (expr.name != "byte") expr.args[i] = arg;
    }
  }

  return exprs;
}


unittest {
  /* Macro "byte" */
  auto program = `.byte 0`;
  auto ptree   = Assembly(program);
  auto exprs   = Exprs(ptree);
  assert(exprs.length == 1);
  auto expr = exprs[0];
  assert(expr.type   == AType.AMacro);
  assert(expr.offset == 0);
  assert(expr.length == 1 * SLOT_SIZE);
  assert(expr.name   == "byte");
  assert(expr.args   == [0]);
  assert(expr.ascii  == "");

  /* Macro "ascii" */
  auto ascii = "heyo!";
  program = `.ascii "heyo!"`;
  ptree   = Assembly(program);
  exprs   = Exprs(ptree);
  assert(exprs.length == 1);
  expr = exprs[0];
  assert(expr.type   == AType.AMacro);
  assert(expr.offset == 0);
  assert(expr.length == ascii.length * SLOT_SIZE);
  assert(expr.name   == "ascii");
  assert(expr.ascii  == "heyo!");

  /* Instruction "add" */
  program = `add 1`;
  ptree   = Assembly(program);
  exprs   = Exprs(ptree);
  assert(exprs.length == 1);
  expr = exprs[0];
  assert(expr.type   == AType.AInstr);
  assert(expr.offset == 0);
  assert(expr.length == 2 * SLOT_SIZE);
  assert(expr.name   == "add");
  assert(expr.args   == [1]);
  assert(expr.ascii  == "");

  /* Label "zero" */
  program = `zero:`;
  ptree   = Assembly(program);
  exprs   = Exprs(ptree);
  assert(exprs == []);

  /* Comment */
  program = `# Just a test`;
  ptree   = Assembly(program);
  exprs   = Exprs(ptree);
  assert(exprs == []);
}


/* Convert list of expressions into a binary,
   which is written to the given file. */
string Binary(AExpr[] exprs, File f) {
  auto binary = "";

  foreach (ref expr; exprs) {
    switch (expr.type) {
      case AType.AInstr:
        int arg = 0;
        if (expr.args.length > 0) arg = expr.args[0];
        string rawSlot1 = format("%05b%011b", lookupOpcode(expr.name), 0);
        string rawSlot2 = format("%016b", cast(short) arg);
        binary ~= rawSlot1 ~ "\n" ~ rawSlot2 ~ "\n";

        for (int i = 0; i < 16; i += 1)
          f.rawWrite([rawSlot1[i] == '1']);

        for (int i = 0; i < 16; i += 1)
          f.rawWrite([rawSlot2[i] == '1']);

        break;
      case AType.AMacro:
        switch (expr.name) {
          case "byte":
            string bytes = format("%016b", cast(short) expr.args[0]);
            binary ~= bytes ~ "\n";
            foreach (char d; bytes) {
              f.rawWrite([d == '1']);
            }
            break;
          case "ascii":
            foreach (char c; expr.ascii) {
              string bytes = format("%016b", cast(short) c);
              binary ~= bytes ~ "\n";
              foreach (char d; bytes) {
                f.rawWrite([d == '1']);
              }
            }
          default:
        }

        break;
      default:
    }
  }

  return binary;
}


unittest {
  /* Throw away test output. */
  auto f = File("/dev/null", "w");

  /* Macro "byte" */
  auto program = `.byte 1`;
  auto ptree   = Assembly(program);
  auto exprs   = Exprs(ptree);
  auto binary  = Binary(exprs, f);
  assert(binary == "0000000000000001\n");

  /* Macro "ascii" */
  program = `.ascii " ~"`; /* ASCII codes 32 and 126 */
  ptree   = Assembly(program);
  exprs   = Exprs(ptree);
  binary  = Binary(exprs, f);
  assert(binary == "0000000000100000\n0000000001111110\n");

  /* Instruction "add" */
  program = `add 1`;
  ptree   = Assembly(program);
  exprs   = Exprs(ptree);
  binary  = Binary(exprs, f);
  assert(binary == "0100000000000000\n0000000000000001\n");
}