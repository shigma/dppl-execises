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
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | TmZero(_) -> t
  | TmIf(_, t1, t2, t3) ->
    let t1' = eval t1 in (
    match t1' with
      | TmTrue(_) -> eval t2
      | TmFalse(_) -> eval t3
      | _ -> t)
  | TmSucc(fi, t1) ->
    let t1' = eval t1 in
    if isnumericval t1' then TmSucc(fi, t1') else t
  | TmPred(_, t1) ->
    let t1' = eval t1 in (
    match t1' with
      | TmZero(_) -> TmZero(dummyinfo)
      | TmSucc(_, nv1) when isnumericval nv1 -> nv1
      | _ -> t)
  | TmIsZero(_, t1) ->
    let t1' = eval t1 in (
    match t1' with
      | TmZero(_) -> TmTrue(dummyinfo)
      | TmSucc(_, nv1) when isnumericval nv1 -> TmFalse(dummyinfo)
      | _ -> t)
