y = lambda f: T -> T.
  (lambda x: (Rec A. A -> T). f ((unfold [Rec A. A -> T] x) x))
  (fold [Rec A. A -> T] (lambda x: (Rec A. A -> T). f ((unfold [Rec A. A -> T] x) x)));
