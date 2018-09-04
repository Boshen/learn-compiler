---- case1.hs
\x y -> let x = if x then x else y in x + 2

---- case1.js
(x) => (y) => const x = () => { x ? x : y; return x(2) }

---- case2.hs
if x
then (if a then b else c)
else z

---- case2.js
x ? (a ? b : c) : z

---- case3.hs
map (\n -> n * 2) (range 1 100)

---- case3.js
map((n) => n * 2)(range(1)(100))

