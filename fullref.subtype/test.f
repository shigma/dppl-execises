Counter = {
  get: Unit -> Nat,
  inc: Unit -> Unit
};

ResetCounter = {
  get: Unit -> Nat,
  inc: Unit -> Unit,
  reset: Unit -> Unit
};

BackupCounter = {
  get: Unit -> Nat,
  inc: Unit -> Unit,
  reset: Unit -> Unit,
  backup: Unit -> Unit
};

SetCounter = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit
};

DecCounter = {
  get: Unit -> Nat,
  inc: Unit -> Unit,
  reset: Unit -> Unit,
  dec: Unit -> Unit
};

InstrCounter = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit,
  accesses: Unit -> Nat
};

CounterRep = {
  x: Ref Nat
};

BackupCounterRep = {
  x: Ref Nat,
  b: Ref Nat
};

InstrCounterRep = {
  x: Ref Nat,
  a: Ref Nat,
  b: Ref Nat
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

backupCounterClass =
  lambda r: BackupCounterRep.
    let super = resetCounterClass r in {
      get = super.get,
      inc = super.inc,
      reset = lambda _: Unit. (r.x := !(r.b)),
      backup = lambda _: Unit. (r.b := !(r.x))
    } as BackupCounter;

setCounterClass =
  lambda r: CounterRep.
    lambda self: Unit -> SetCounter.
      lambda _: Unit. {
        get = lambda _: Unit. !(r.x),
        set = lambda i: Nat. r.x := i,
        inc = lambda _: Unit. (self unit).set (succ ((self unit).get unit))
      } as SetCounter;

decCounterClass =
  lambda r: CounterRep.
    let super = resetCounterClass r in {
      get = super.get,
      inc = super.inc,
      reset = super.reset,
      dec = lambda _: Unit. r.x := pred (!(r.x))
    } as DecCounter;

instrCounterClass =
  lambda r: InstrCounterRep.
    lambda self: Unit -> InstrCounter.
      lambda _: Unit.
        let super = setCounterClass r self unit in {
          get = lambda _: Unit. (r.a := succ (!(r.a)); super.get unit),
          set = lambda i: Nat. (r.a := succ (!(r.a)); super.set i),
          inc = super.inc,
          accesses = lambda _: Unit. !(r.a)
        } as InstrCounter;

ResetInstrCounter = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit,
  reset: Unit -> Unit,
  accesses: Unit -> Nat
};

resetInstrCounterClass =
  lambda r: InstrCounterRep.
    lambda self: Unit -> ResetInstrCounter.
      lambda _: Unit.
        let super = instrCounterClass r self unit in {
          get = super.get,
          set = super.set,
          inc = super.inc,
          reset = lambda _: Unit. (r.x := 0),
          accesses = super.accesses
        } as ResetInstrCounter;

BackupInstrCounter = {
  get: Unit -> Nat,
  set: Nat -> Unit,
  inc: Unit -> Unit,
  reset: Unit -> Unit,
  backup: Unit -> Unit,
  accesses: Unit -> Nat
};

backupInstrCounterClass =
  lambda r: InstrCounterRep.
    lambda self: Unit -> ResetInstrCounter.
      lambda _: Unit.
        let super = resetInstrCounterClass r self unit in {
          get = super.get,
          set = super.set,
          inc = super.inc,
          reset = lambda _: Unit. (r.x := !(r.b)),
          backup = lambda _: Unit. (r.b := !(r.x)),
          accesses = super.accesses
        } as BackupInstrCounter;

newBackupInstrCounter =
  lambda _: Unit.
    let r = { x = ref 1, a = ref 0, b = ref 0 } in
      fix (backupInstrCounterClass r) unit;
