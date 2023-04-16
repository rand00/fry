let mk1 x = x
let mk2 a b = a, b
let mk3 a b c = a, b, c
let mk4 a b c d = a, b, c, d
let mk5 a b c d e = a, b, c, d, e
let mk6 a b c d e f = a, b, c, d, e, f

let eq1 eq0 x y = eq0 x y
let eq2 eq0 eq1 (x, y) (x', y') =
  eq0 x x' && eq1 y y'
let eq3 eq0 eq1 eq2 (x, y, z) (x', y', z') =
  eq0 x x' && eq1 y y' && eq2 z z'
let eq4 eq0 eq1 eq2 eq3 (x, y, z, w) (x', y', z', w') =
  eq0 x x' && eq1 y y' && eq2 z z' && eq3 w w'

