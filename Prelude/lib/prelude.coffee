module.exports =
  # extend :: Object -> Object -> Object
  # Extend objA with all of objB's properties
  extend: (objA, objB) ->
    objA[k] = v for k, v of objB
    objA

  # globalize :: Object -> Object
  # Promote obj's properties to the global scope
  globalize: (obj) -> @extend global, obj

  show: console.log # Alias

  # curry :: Function -> Object... -> Function
  # Create a new function by binding preArgs from left to right
  curry: (fn, preArgs...) ->
    return (args...) ->
      fn.apply this, preArgs.concat(args)

  lcurry: @curry # Alias

  # rcurry :: Function -> Object... -> Function
  # Create a new function by binding postArgs from right to left
  rcurry: (fn, postArgs...) ->
    return (args...) ->
      fn.apply this, args.concat(@reverse postArgs)

  # reverse :: [Object] -> [Object]
  # Return a new list with the order of the items reversed
  reverse: (lst) -> lst.reverse()

  # negate :: Number -> Number
  # Unary minus (i.e. not not)
  negate: (x) -> return -1 * x

  abs: Math.abs # Alias

  # signum :: Number -> Number
  # Calculate the sign of a number
  signum: (x) ->
    return 0 if x is 0
    return 1 if x > 0
    return -1

  # mod :: Number -> Number -> Number
  # Calculate the modulus (TODO rem)
  mod: (a, b) -> a % b

  pi: Math.PI # Alias

  floor: Math.floor # Alias

  ceiling: Math.ceil # Alias

  add: (x, y) -> x + y # Alias

  subtract: (x, y) -> x - y # Alias

  square: (x) -> x * x # Alias

  # even :: Number -> Boolean
  # Test for even-ness
  even: (x) -> x % 2 is 0

  # odd :: Number -> Boolean
  # Test for odd-ness
  odd: (x) -> not @even x

  # implies :: Boolean -> Boolean -> Boolean
  # True can't imply false. That's it.
  implies: (a, b) ->
    return false if a is true and b is false
    true

  # cons :: Object -> [Object] -> [Object]
  # List constructor
  cons: (a, lst) -> [a].concat lst

  concat: (a, b) -> a.concat b # Alias

  # map :: [Object] -> (Object -> Object) -> [Object]
  # Apply fn to each item in lst
  map: (lst, fn) ->
    res = []
    res.push fn item for item in lst
    res

  # filter :: (Object -> Boolean) -> [Object] -> [Object]
  # Keep all items in lst satisfying pred
  filter: (pred, lst) ->
    res = []
    res.push item if pred item for item in lst
    res

  # foldl :: (Object -> Object -> Object) -> Object -> [Object] -> Object
  # Apply fn LTR across lst, consuming items in pairs, starting off with acc
  foldl: (fn, acc, lst) ->
    f = @curry fn, acc
    for item in lst
      acc = f item
      f   = @curry fn, acc
    acc

  # foldl1 :: (Object -> Object -> Object) -> [Object] -> Object
  # Like foldl, but take head(lst) as the accumulator argument, acc
  foldl1: (fn, lst) ->
    acc = @head lst
    f   = @curry fn, acc
    for item in tail lst
      acc = f item
      f   = @curry fn, acc
    acc

  # foldr :: (Object -> Object -> Object) -> Object -> [Object] -> Object
  # Like foldl, but RTL
  foldr: (fn, acc, lst) ->
    @foldl fn, acc, (@reverse lst)

  # foldr1 :: (Object -> Object -> Object) -> [Object] -> Object
  # Like foldl1, but RTL
  foldr1: (fn, lst) -> @foldl1 fn, (@reverse lst)

  # head :: [Object] -> Object
  # Return the first item in lst
  head: (lst) -> lst[0]

  # tail :: [Object] -> [Object]
  # Return all but the first item in lst
  tail: (lst) -> lst.slice 1

  fst: @head # Alias (JavaScript doesn't have tuples)

  # snd :: [Object] -> Object
  # Return the second item in lst
  snd: (lst) -> lst[1]

  # init :: [Object] -> [Object]
  # Return all but the last item in lst
  init: (lst) -> @take lst.length - 1, lst

  # take :: Number -> [Object] -> [Object]
  # Return the first n items in lst
  take: (n, lst) -> lst.slice 0, n

  # drop :: Number -> [Object] -> [Object]
  # Return lst without the first n items
  drop: (n, lst) -> lst.slice n

  # zip :: [Object] -> [Object] -> [[Object, Object]]
  # Join two lists into one another, zipper-like
  zip: (lstA, lstB) ->
    res = []
    for a, i in lstA
      res[i] = [lstA[i], lstB[i]]
    res

  # id :: Object -> Object
  # Return obj
  id: (obj) -> obj
