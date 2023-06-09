/* Examples for testing */

 
Pair = lambda X. lambda Y. All R. (X -> Y -> R) -> R;

pair = lambda X. lambda Y. lambda x: X. lambda y: Y. lambda R. lambda p: X -> Y -> R. p x y;

f = lambda X.lambda Y.lambda f:Pair X Y. f;

fst = lambda X.lambda Y.lambda p:Pair X Y.p [X] (lambda x:X.lambda y:Y.x);
snd = lambda X.lambda Y.lambda p:Pair X Y.p [Y] (lambda x:X.lambda y:Y.y);

pr = pair [Nat] [Bool] 0 false;
fst [Nat] [Bool] pr;
snd [Nat] [Bool] pr;

List = lambda X. All R. (X -> R -> R) -> R -> R; 

diverge =
lambda X.
  lambda _:Unit.
  fix (lambda x:X. x);

nil = lambda X.
      (lambda R. lambda c:X->R->R. lambda n:R. n)
      as List X; 

cons = 
lambda X.
  lambda hd:X. lambda tl: List X.
    (lambda R. lambda c:X->R->R. lambda n:R. c hd (tl [R] c n))
    as List X; 

isnil =  
lambda X. 
  lambda l: List X. 
    l [Bool] (lambda hd:X. lambda tl:Bool. false) true; 

head = 
lambda X. 
  lambda l: List X. 
    (l [Unit->X] (lambda hd:X. lambda tl:Unit->X. lambda _:Unit. hd) (diverge [X]))
    unit; 

tail =  
lambda X.  
  lambda l: List X. 
    (fst [List X] [List X] ( 
      l [Pair (List X) (List X)]
        (lambda hd: X. lambda tl: Pair (List X) (List X). 
          pair [List X] [List X] 
            (snd [List X] [List X] tl)  
            (cons [X] hd (snd [List X] [List X] tl))) 
        (pair [List X] [List X] (nil [X]) (nil [X]))))
    as List X; 
 

"hello";

unit;

lambda X. lambda x:X. x; 
(lambda X. lambda x:X. x) [All X.X->X]; 

 {*All Y.Y, lambda x:(All Y.Y). x} as {Some X,X->X};


lambda x:Bool. x;
(lambda x:Bool->Bool. if x false then true else false) 
  (lambda x:Bool. if x then false else true); 

lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0); 

{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 


{*Nat, {c=0, f=lambda x:Nat. succ x}}
  as {Some X, {c:X, f:X->Nat}};
let {X,ops} = {*Nat, {c=0, f=lambda x:Nat. succ x}}
              as {Some X, {c:X, f:X->Nat}}
in (ops.f ops.c);


T = Nat->Nat;
lambda f:T. lambda x:Nat. f (f x);


timesfloat 2.0 3.14159;

let x=true in x;

lambda x:A. x;
