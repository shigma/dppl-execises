/* Examples for testing */

NatList = <nil:Unit, cons:{Nat,NatList} >;
nil = <nil=unit> as NatList;
cons = lambda n:Nat. lambda l:NatList. <cons={n, l}> as NatList;

/* NatList is a recursive type, and the type checker cannot handle recursive types. */
