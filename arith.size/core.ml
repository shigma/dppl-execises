open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval t = match t with
    TmTrue(_) -> 1
  | TmFalse(_) -> 1
  | TmZero(_) -> 1
  | TmSucc(_, t1) -> 1 + eval t1
  | TmPred(_, t1) -> 1 + eval t1
  | TmIsZero(_, t1) -> 1 + eval t1
  | TmIf(_, t1, t2, t3) -> 1 + eval t1 + eval t2 + eval t3
