/* Examples for testing */

equal = fix (lambda eq: Nat -> Nat -> Bool.
  lambda m: Nat. lambda n: Nat.
    if iszero m then iszero n
    else if iszero n then false
    else eq (pred m) (pred n));

plus = fix (lambda p: Nat -> Nat -> Nat.
  lambda m: Nat. lambda n: Nat.
    if iszero m then n else succ (p (pred m) n));

plus 10 105;
plus 0 1;
plus 0 0;
plus 2 0;
