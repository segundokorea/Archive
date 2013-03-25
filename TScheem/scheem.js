var parser = require('./parser');
var evaluator = require('./eval');

if (process.argv[2]) {
  var program = require('fs').readFileSync(process.argv[2], 'utf-8');
  var reading = parser.parse(program);
  var result  = evaluator.evaluate(reading, evaluator.prelude);
  if (result !== undefined) console.log('===>', result[0]);
  process.exit(0); 
}

var readline = require('readline');
var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.setPrompt("%> ", 3);

var val = undefined;
var env = evaluator.prelude;

rl.on('line', function(input) {
  if (input === 'quit') {
    console.log("Bye!");
    process.exit(0);
  }
  var reading = parser.parse(input);
  var res = evaluator.evaluate(reading, env);
  if (res !== undefined) {
    val = res[0]; env = res[1];
    if (val !== undefined) console.log('=>', val);
  }

  rl.prompt();
});

rl.prompt();