// Common functions
module.exports.prelude = {
  bindings: {

    'id': function(x) { return x; },

    '+':  function(x,y) { return x + y; },
    '-':  function(x,y) { return x - y; },
    '*':  function(x,y) { return x * y; },
    '/':  function(x,y) { return x / y; },
    '<':  function(x,y) { return x <  y; },
    '<=': function(x,y) { return x <= y; },
    '>':  function(x,y) { return x >  y; },
    '>=': function(x,y) { return x >= y; },
    '=':  function(x,y) { return x == y; },

    'cons': function(elem, l) { l.unshift(elem); return l; },

    'car': function(l) { return l[0]; },

    'cdr': function(l) { return l.slice(1); },

    'map': function(f, l) {
      var res = [];
      for (var i in l) {
        var val = f.apply(this, [l[i]]);
        res.push(val);
      }; return res;
    },

    'display': function(x) {
      if (x === null) { console.log('nil'); return undefined; }
      if (x.val) { console.log(x.val); return undefined; }
      console.log(x); return undefined;
    },

    'builtin.fact': function fact(n) {
      if (n <= 1) { return 1; }
      else { return n * fact(n-1); }
    }

  }, outer: {}
};



module.exports.evaluate = function(reading, env) {
  if (env === undefined) env = {};

  // Helpers
  var Quote = require('./syntax').Quote;

  var UnboundNameError = function(v) { return "unbound name " + v; };

  var lookup = function(v, e) {
    if (e === undefined) e = { bindings: {}, outer: env };
    if (e.bindings === undefined) return undefined;
    for (var name in e.bindings) {
      if (v == name) return e.bindings[v];
    } return lookup(v, e.outer);
  };

  var update = function(v, val, e) {
    if (e === undefined) e = { bindings: {}, outer: env };
    if (e.bindings === undefined) throw UnboundNameError(v);

    if (e.bindings[v]) {
      e.bindings[v] = val;
      env = e;
      return env;
    }

    e.outer = update(v, val, e.outer);
    env = e;
    return env;
  };

  var ignore = function(status, val, note) {
    if (typeof note !== 'string') note = '';
    return {
      tag: undefined,
      status: status,
      note: note,
      val: val,
      env: env
    };
  };

  var success = function(val, note) { return ignore('successful', val, note); };

  var failure = function(val, note) { return ignore('failure', val, note); };




  // The Meat

  var evaluate = function(expr) {
    // Primitives:
    // Try to look up atoms, otherwise
    // just return the primitive value
    switch (expr.tag) {
      case 'quote':
        return evaluate(expr.val);
      case 'atom':
        var val = lookup(expr.val);
        if (val) return success(val);
      case 'text':
      case 'nil':
      case 'bool':
      case 'char':
      case 'num':
        return success(expr.val);
    }


    // List Form:
    // Try evaluating any special forms
    // Otherwise, return the list itself
    expr = expr.val;
    switch (expr[0].val) {

      case 'set!':
        // Update an existing binding
        var val = evaluate(expr[2]).val;
        update(expr[1].val, val);
        return success(undefined);


      case 'if':
        // Conditional evaluation
        var cond = evaluate(expr[1]).val;
        if (cond) return evaluate(expr[2]);
        return evaluate(expr[3]);


      case 'let':
        expr.shift();
        var finalExpr = expr.pop();

        // Add bindings to a new env where the final
        // expression will be evaluated
        var newEnv = { bindings: {}, outer: env };
        var args = [];
        for (var i = 0; i < expr.length; ++i) {
          var binding = expr[i].val;
          newEnv.bindings[binding[0].val] = binding[1].val;
        }

        // Execute in overloaded env, but return the
        // current env back to it's original state
        env = newEnv;
        var val = evaluate(finalExpr).val
        env = env.outer;
        return success(val);


      case 'lambda':
        var lambda = function() {
          // First, evaluate the args in the current env
          // Bind those args to a new env for the lambda
          var args = expr[1];
          var lambdaEnv = { bindings: {}, outer: this };
          for (i in args.val) {
            lambdaEnv.bindings[args.val[i].val] = arguments[i];
          }

          // Execute the lambda in the overloaded env
          env = lambdaEnv;
          var val = evaluate(expr[2]).val;

          // Return the env back to normal
          env = lambdaEnv.outer;
          return val;
        };

        return success(lambda);


      case 'define':
        // Amend the current env with a new binding
        var res = evaluate(expr[2]);
        var newEnv = { bindings: {}, outer: env };
        newEnv.bindings[expr[1].val] = res.val;
        env = newEnv;
        return success(undefined);


      case 'begin':
        // Evaluate a list of expressions in order
        expr.shift();
        var val, res;
        while (expr.length > 0) {
          res = evaluate(expr.shift());
          val = res === undefined ? undefined : res.val;
        } return success(val);


      default:
        // Try to find our func in the current env
        var func = lookup(expr[0].val);

        // If the head of the list isn't an atom or
        // doesn't exist in the current env, then
        // treat the expr as a regular list and just
        // evaluate each element.
        if (func === undefined) {
          var list = [];
          for (var i in expr) {
            list.push(evaluate(expr[i]).val);
          }
          return success(list);
        }

        // Now we know we've got a function, so go
        // ahead an load up any arguments
        var args = [];
        for (var i = 1; i < expr.length; ++i) {
          args.push(evaluate(expr[i]).val);
        }

        // Here's the trick: inside our func, "this"
        // will point to the current env, not the env
        // where the func was defined. Lambda exploits
        // this fact to implement recursion.
        var res = func.apply(env, args);
        return success(res);
    }

  };


  if (reading.tag === 'list') return [evaluate(reading).val, env];
  return undefined;
};