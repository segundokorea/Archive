prelude = require '../lib/prelude'

prelude.globalize prelude

inc = curry add, 1

dec = rcurry subtract, 1

a = [1, 2, 3]

show floor negate negate pi

show ceiling 1.5

show even 2

show head filter even, map a, inc

show snd a

show tail a

show init a

show take 2, a

show dec 3

show foldl add, 1, a

show foldl1 add, a

show foldr subtract, 10, a

show foldr1 add, a

show drop 1, a

show zip a, a

show implies true, false

show cons 0, a
