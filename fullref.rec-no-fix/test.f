/* Examples for testing */

plus = ref (lambda m: Nat. lambda n: Nat. 0);

plus := (lambda m: Nat. lambda n: Nat.
  if iszero m then n else succ ((!plus) (pred m) n));

!plus 10 105;
!plus 0 1;
!plus 0 0;
!plus 2 0;
