let mk1 x = x
let mk2 a b = a, b
let mk3 a b c = a, b, c
let mk4 a b c d = a, b, c, d
let mk5 a b c d e = a, b, c, d, e
let mk6 a b c d e f = a, b, c, d, e, f

let eq1 eq_f0 x y = eq_f0 x y
let eq2 eq_f0 eq_f1 (x, y) (x', y') =
  eq_f0 x x' && eq_f1 y y'
let eq3 eq_f0 eq_f1 eq_f2 (x, y, z) (x', y', z') =
  eq_f0 x x' && eq_f1 y y' && eq_f2 z z'
let eq4 eq_f0 eq_f1 eq_f2 eq_f3 (x, y, z, w) (x', y', z', w') =
  eq_f0 x x' && eq_f1 y y' && eq_f2 z z' && eq_f3 w w'
let eq5 eq_f0 eq_f1 eq_f2 eq_f3 eq_f4 (x, y, z, w, q) (x', y', z', w', q') =
  eq_f0 x x' && eq_f1 y y' && eq_f2 z z' && eq_f3 w w' && eq_f4 q q'
let eq6 eq_f0 eq_f1 eq_f2 eq_f3 eq_f4 eq_f5
    (x, y, z, w, q, l) (x', y', z', w', q', l')
  =
  eq_f0 x x' && eq_f1 y y' && eq_f2 z z' && eq_f3 w w' && eq_f4 q q' &&
  eq_f5 l l'
    

