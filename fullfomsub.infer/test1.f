/* Examples for testing */
 
id = lambda X. lambda x: X. x;
id 1;

f = lambda x: Nat. lambda Y. lambda y: Y. y;
g = lambda Y. lambda x: Nat. lambda y: Y. y;
f 1;
g 1;

Pair = lambda X. lambda Y. All R. (X -> Y -> R) -> R;
pair =
  lambda X. lambda x: X.
    lambda Y. lambda y: Y.
      (lambda R. lambda p: X -> Y -> R. p x y) as Pair X Y;

fst = lambda X. lambda Y. lambda p: Pair X Y. p (lambda x: X. lambda y: Y. x);
snd = lambda X. lambda Y. lambda p: Pair X Y. p (lambda x: X. lambda y: Y. y);

pr = pair 0 false;
fst pr;
snd pr;

List = lambda X. All R. (X -> R -> R) -> R -> R; 

nil = lambda X. (lambda R. lambda c: X -> R -> R. lambda n: R. n) as List X;

cons = lambda X. lambda hd: X. lambda tl: List X.
  (lambda R. lambda c: X -> R -> R. lambda n: R. c hd (tl c n)) as List X; 

isnil = lambda X. lambda l: List X. l (lambda hd: X. lambda tl: Bool. false) true; 
