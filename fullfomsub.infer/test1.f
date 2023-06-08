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
