/* Examples for testing */
 
Pair = lambda X. lambda Y. All R. (X -> Y -> R) -> R;

pair = lambda X. lambda Y. lambda x: X. lambda y: Y. lambda R. lambda p: X -> Y -> R. p x y;

fst = lambda X. lambda Y. lambda p: Pair X Y. p [X] (lambda x: X. lambda y: Y. x);
snd = lambda X. lambda Y. lambda p: Pair X Y. p [Y] (lambda x: X. lambda y: Y. y);

pr = pair [Nat] [Bool] 0 false;
fst [Nat] [Bool] pr;
snd [Nat] [Bool] pr;

List = lambda X. Dep n: Nat. All R. (X -> R -> R) -> R -> R;

diverge = lambda X. lambda _: Unit. fix (lambda x: X. x);

nil = lambda X. (lambda R. lambda _: X -> R -> R. lambda n: R. n) as List X [0];

cons = lambda X. Dep n: Nat. lambda hd: X. lambda tl: List X [n].
  (lambda R. lambda c: X -> R -> R. lambda n: R. c hd (tl [R] c n)) as List X [succ n]; 

isnil = lambda X. Dep n: Nat. lambda l: List X [n].
  l [iszero n] (lambda hd: X. lambda tl: Bool. false) true; 

head = lambda X. lambda l: List X. 
  (l [Unit -> X] (lambda hd: X. lambda tl: Unit -> X. lambda _: Unit. hd) (diverge [X])) unit; 

tail =  lambda X. lambda l: List X [0]. 
  (fst [List X [0]] [List X [0]] ( 
    l [Pair (List X [0]) (List X [0])]
      (lambda hd: X. lambda tl: Pair (List X [0]) (List X [0]). 
        pair [List X [0]] [List X [0]] 
          (snd [List X [0]] [List X [0]] tl)  
          (cons [X] hd (snd [List X [0]] [List X [0]] tl))) 
      (pair [List X [0]] [List X [0]] (nil [X]) (nil [X])))) as List X [0]; 

ls = cons [Nat] 1 (cons [Nat] 2 (cons [Nat] 3 (nil [Nat])));
isnil [Nat] ls;
head [Nat] ls;
head [Nat] (tail [Nat] ls);
head [Nat] (tail [Nat] (tail [Nat] ls));
isnil [Nat] (tail [Nat] (tail [Nat] (tail [Nat] ls)));


/*
map = lambda X. lambda Y. fix (lambda map: (X -> Y) -> List X -> List Y.
  lambda f: X -> Y. lambda l: List X.
    (l [List Y] (
      lambda hd: X. lambda tl: List X.
        cons [Y] (f hd) (map f tl)) nil [Y]) as List Y);
*/
