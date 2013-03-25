module.exports.Comment = function(c) {
  return { tag: 'comment', val: c };
};

module.exports.Quote = function(e) {
  return { tag: 'quote', val: e };
};

module.exports.Num = function(n) {
  return { tag: 'num', val: n };
};

module.exports.List = function(es) {
  var list = [];
  for (var i in es) { // Skip comments
    if (es[i].tag !== undefined) list.push(es[i]);
  } return { tag: 'list', val: list };
};

module.exports.Bool = function(cond) {
  return { tag: 'bool', val: cond };
};

module.exports.Nil = function() {
  return { tag: 'nil', val: null };
};

module.exports.Char = function(c) {
  return { tag: 'char', val: c };
};

module.exports.Atom = function(a) {
  return { tag: 'atom', val: a };
};

module.exports.Text = function(s) {
  return { tag: 'text', val: s };
};
