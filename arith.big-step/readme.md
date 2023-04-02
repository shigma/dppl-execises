# big-step

Change the evaluation to use big-step semantics, and compute the following expressions:

```
true;
if false then true else false;
if 0 then 1 else 2;
if true then (succ false) else 2;
0;
succ (pred 0);
iszero (pred (succ (succ 0)));
```
