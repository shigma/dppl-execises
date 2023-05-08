/* Examples for testing */

Counter = {
  get: Unit -> Nat,
  inc: Unit -> Unit
};

ResetCounter = {
  get: Unit -> Nat,
  inc: Unit -> Unit,
  reset: Unit -> Unit
};

DecCounter = {
  get: Unit -> Nat,
  inc: Unit -> Unit,
  reset: Unit -> Unit,
  dec: Unit -> Unit
};

CounterRep = {
  x: Ref Nat
};

counterClass =
  lambda r: CounterRep. {
    get = lambda _: Unit. !(r.x),
    inc = lambda _: Unit. r.x := succ (!(r.x))
  } as Counter;

resetCounterClass =
  lambda r: CounterRep.
    let super = counterClass r in {
      get = super.get,
      inc = super.inc,
      reset = lambda _: Unit. r.x := 1
    } as ResetCounter;

decCounterClass =
  lambda r: CounterRep.
    let super = resetCounterClass r in {
      get = super.get,
      inc = super.inc,
      reset = super.reset,
      dec = lambda _: Unit. r.x := pred (!(r.x))
    } as DecCounter;
