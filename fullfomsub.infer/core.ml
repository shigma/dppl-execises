open Format
open Syntax
open Support.Error
open Support.Pervasive

exception NoRuleApplies

(* ------------------------   EVALUATION  ------------------------ *)

let rec isnumericval ctx t = match t with
  | TmZero(_) -> true
  | TmSucc(_, t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
  | TmString _  -> true
  | TmPack(_, _, v1, _) when isval ctx v1 -> true
  | TmTAbs(_, _, _, _) -> true
  | TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval ctx t -> true
  | TmAbs(_, _, _, _) -> true
  | TmRecord(_, fields) -> List.for_all (fun (l, ti) -> isval ctx ti) fields
  | TmUnit(_)  -> true
  | TmFloat _  -> true
  | _ -> false

let rec tmfree ctx x t = match t with
  | TmVar(_, y, _) -> false
  | TmAbs(_, y, _, t2) ->
      if x = y then true else tmfree ctx x t2
  | _ -> true

let rec eval1 ctx t = match t with
  | TmApp(fi, TmAbs(_, x, tyT11, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi, TmTAbs(fi2, x, tyT11, t1), t2) ->
      let t = eval1 ctx (TmApp(fi, t1, t2)) in
      if tmfree ctx x t then t
      else TmTAbs(fi2, x, tyT11, t)
  | TmApp(fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmAscribe(fi, v1, tyT) when isval ctx v1 ->
      v1
  | TmAscribe(fi, t1, tyT) ->
      let t1' = eval1 ctx t1 in
      TmAscribe(fi, t1', tyT)
  | TmTApp(fi, TmTAbs(_, x, _, t11), tyT2) ->
      tytermSubstTop tyT2 t11
  | TmTApp(fi, t1, tyT2) ->
      let t1' = eval1 ctx t1 in
      TmTApp(fi, t1', tyT2)
  | TmRecord(fi, fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l, vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l, vi)::rest'
      | (l, ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, TmRecord(_, fields), l) ->
      (try List.assoc l fields
      with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmUnpack(fi, _, _, TmPack(_, tyT11, v12, _), t2) when isval ctx v12 ->
      tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
  | TmUnpack(fi, tyX, x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmUnpack(fi, tyX, x, t1', t2)
  | TmPack(fi, tyT1, t2, tyT3) ->
      let t2' = eval1 ctx t2 in
      TmPack(fi, tyT1, t2', tyT3)
  | TmIf(_, TmTrue(_), t2, t3) ->
      t2
  | TmIf(_, TmFalse(_), t2, t3) ->
      t3
  | TmIf(fi, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi, t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_, TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_, TmSucc(_, nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi, t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_, TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_, TmSucc(_, nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi, t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | TmVar(fi, n, _) ->
      (match getbinding fi ctx n with
        | TmAbbBind(t, _) -> t 
        | _ -> raise NoRuleApplies)
  | TmTimesfloat(fi, TmFloat(_, f1), TmFloat(_, f2)) ->
      TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi, (TmFloat(_, f1) as t1), t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi, t1, t2') 
  | TmTimesfloat(fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi, t1', t2) 
  | TmLet(fi, x, v1, t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi, x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | TmFix(fi, v1) as t when isval ctx v1 ->
      (match v1 with
      | TmAbs(_, _, _, t12) -> termSubstTop t t12
      | _ -> raise NoRuleApplies)
  | TmFix(fi, t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi, t1')
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   KINDING  ------------------------ *)

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
  | TyAbbBind(tyT, _) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
  | TyAbbBind(tyT, _) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
  | TyApp(TyAbs(_, _, tyT12), tyT2) -> typeSubstTop tyT2 tyT12
  | TyVar(i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  let tyT = 
    match tyT with
      | TyApp(tyT1, tyT2) -> TyApp(simplifyty ctx tyT1, tyT2)
      | tyT -> tyT
  in 
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) ->
      (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyAbs(tyX1, knKS1, tyS2), TyAbs(_, knKT1, tyT2)) ->
      ((=) knKS1 knKT1) &&
       (let ctx = addname ctx tyX1 in
        tyeqv ctx tyS2 tyT2)
  | (TyApp(tyS1, tyS2), TyApp(tyT1, tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyBool, TyBool) -> true
  | (TyNat, TyNat) -> true
  | (TyRecord(fields1), TyRecord(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (li2, tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TySome(tyX1, tyS1, tyS2), TySome(_, tyT1, tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       tyeqv ctx tyS1 tyT1 && tyeqv ctx1 tyS2 tyT2
  | (TyUnit, TyUnit) -> true
  | (TyId(b1), TyId(b2)) -> b1=b2
  | (TyVar(i, _), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i, _)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i, _), TyVar(j, _)) -> i=j
  | (TyAll(tyX1, tyS1, tyS2, _), TyAll(_, tyT1, tyT2, _)) ->
       let ctx1 = addname ctx tyX1 in
       tyeqv ctx tyS1 tyT1 && tyeqv ctx1 tyS2 tyT2
  | (TyString, TyString) -> true
  | (TyFloat, TyFloat) -> true
  | (TyTop, TyTop) -> true
  | _ -> false

let rec getkind fi ctx i =
  match getbinding fi ctx i with
    | TyVarBind(tyT) -> kindof ctx tyT
    | TyAbbBind(_, Some(knK)) -> knK
    | TyAbbBind(_, None) -> error fi ("No kind recorded for variable "
                                      ^ (index2name fi ctx i))
    | _ -> error fi ("getkind: Wrong kind of binding for variable " 
                      ^ (index2name fi ctx i))

and kindof ctx tyT = match tyT with
  | TyArr(tyT1, tyT2) ->
      if kindof ctx tyT1 <> KnStar then error dummyinfo "star kind expected";
      if kindof ctx tyT2 <> KnStar then error dummyinfo "star kind expected";
      KnStar
  | TyVar(i, _) ->
      let knK = getkind dummyinfo ctx i
      in knK
  | TyAbs(tyX, knK1, tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind(maketop knK1)) in
      let knK2 = kindof ctx' tyT2 in
      KnArr(knK1, knK2)
  | TyApp(tyT1, tyT2) ->
      let knK1 = kindof ctx tyT1 in
      let knK2 = kindof ctx tyT2 in
      (match knK1 with
        | KnArr(knK11, knK12) ->
            if (=) knK2 knK11 then knK12
            else error dummyinfo "parameter kind mismatch"
        | _ -> error dummyinfo "arrow kind expected")
  | TyAll(tyX, tyT1, tyT2, _) ->
      let ctx' = addbinding ctx tyX (TyVarBind tyT1) in
      if kindof ctx' tyT2 <> KnStar then error dummyinfo "Kind * expected";
      KnStar
  | TyRecord(fldtys) ->
      List.iter (fun (l, tyS) ->
             if kindof ctx tyS<>KnStar then error dummyinfo "Kind * expected")
        fldtys;
      KnStar
  | TySome(tyX, tyT1, tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
      if kindof ctx' tyT2 <> KnStar then error dummyinfo "Kind * expected";
      KnStar
  | _ -> KnStar

let checkkindstar fi ctx tyT = 
  let k = kindof ctx tyT in
  if k = KnStar then ()
  else error fi "Kind * expected"

(* ------------------------   SUBTYPING  ------------------------ *)

let rec promote ctx t = match t with
  | TyVar(i, _) ->
      (match getbinding dummyinfo ctx i with
        | TyVarBind(tyT) -> tyT
        | _ -> raise NoRuleApplies)
  | TyApp(tyS, tyT) -> TyApp(promote ctx tyS, tyT)
  | _ -> raise NoRuleApplies

let rec subtype ctx tyS tyT =
  tyeqv ctx tyS tyT ||
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | (TyVar(_, _), _) -> subtype ctx (promote ctx tyS) tyT
  | (_, TyTop) -> 
      true
  | (TyAll(tyX1, tyS1, tyS2, _), TyAll(_, tyT1, tyT2, _)) ->
      (subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) &&
      let ctx1 = addbinding ctx tyX1 (TyVarBind(tyT1)) in
      subtype ctx1 tyS2 tyT2
  | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) ->
      (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
  | (TyRecord(fS), TyRecord(fT)) ->
      List.for_all
        (fun (li, tyTi) -> 
          try let tySi = List.assoc li fS in
              subtype ctx tySi tyTi
          with Not_found -> false)
        fT
  | (TySome(tyX1, tyS1, tyS2), TySome(_, tyT1, tyT2)) ->
      (subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) &&
      let ctx1 = addbinding ctx tyX1 (TyVarBind(tyT1)) in
      subtype ctx1 tyS2 tyT2
  | (TyAbs(tyX, knKS1, tyS2), TyAbs(_, knKT1, tyT2)) ->
      (=) knKS1 knKT1 &&
      let ctx = addbinding ctx tyX (TyVarBind(maketop knKS1)) in
      subtype ctx tyS2 tyT2
  | (TyApp(_, _), _) -> subtype ctx (promote ctx tyS) tyT
  | (_, _) -> false

let rec lcst ctx tyS =
  let tyS = simplifyty ctx tyS in
  try lcst ctx (promote ctx tyS)
  with NoRuleApplies -> tyS

let rec join ctx tyS tyT =
  if subtype ctx tyS tyT then tyT else 
  if subtype ctx tyT tyS then tyS else
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | (TyAll(tyX, tyS1, tyS2, a1), TyAll(_, tyT1, tyT2, a2)) ->
      if not ((=) a1 a2 && subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) then TyTop
      else 
        let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
        TyAll(tyX, tyS1, join ctx' tyT1 tyT2, a1)
  | (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li, _) -> li) fS in
      let labelsT = List.map (fun (li, _) -> li) fT in
      let commonLabels = 
        List.find_all (fun l -> List.mem l labelsT) labelsS in
      let commonFields = 
        List.map (fun li -> 
                    let tySi = List.assoc li fS in
                    let tyTi = List.assoc li fT in
                    (li, join ctx tySi tyTi))
                 commonLabels in
      TyRecord(commonFields)
  | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) ->
      (try TyArr(meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
        with Not_found -> TyTop)
  | _ -> 
      TyTop

and meet ctx tyS tyT =
  if subtype ctx tyS tyT then tyS else 
  if subtype ctx tyT tyS then tyT else 
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | (TyAll(tyX, tyS1, tyS2, a1), TyAll(_, tyT1, tyT2, a2)) ->
      if not ((=) a1 a2 && subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) then
        raise Not_found
      else 
        let ctx' = addbinding ctx tyX (TyVarBind(tyT1)) in
        TyAll(tyX, tyS1, meet ctx' tyT1 tyT2, a1)
  | (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li, _) -> li) fS in
      let labelsT = List.map (fun (li, _) -> li) fT in
      let allLabels = 
        List.append
          labelsS 
          (List.find_all 
            (fun l -> not (List.mem l labelsS)) labelsT) in
      let allFields = 
        List.map (fun li -> 
                    if List.mem li allLabels then
                      let tySi = List.assoc li fS in
                      let tyTi = List.assoc li fT in
                      (li, meet ctx tySi tyTi)
                    else if List.mem li labelsS then
                      (li, List.assoc li fS)
                    else
                      (li, List.assoc li fT))
                 allLabels in
      TyRecord(allFields)
  | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) ->
      TyArr(join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
  | _ -> 
      raise Not_found

(* ------------------------   TYPING  ------------------------ *)

let rec printTbl ctx tbl = match tbl with
  | [] -> ()
  | (a, b)::ls -> pr ("[table] " ^ string_of_int a ^ ": "); printty ctx b; pr "\n"; printTbl ctx ls

let tblShift d tbl = List.map (fun (i, x, ty) -> (i+d, x, ty)) tbl

let addToTbl tbl x ty = (0, x, ty)::(tblShift 1 tbl)

let rec substTbl ty tbl =
  match tbl with
  | [] -> ty
  | (i, tyX, tyS)::ls when tyX = "" -> substTbl (typeSubst tyS i ty) ls
  | (i, tyX, tyS)::ls -> substTbl ty ls

let rec typeauto ctx tbl ty =
  match tbl with
  | [] -> ty
  | (i, tyX, _)::ls when tyX = "" -> typeauto ctx ls (typeShift (-1) ty)
  | (i, tyX, tyS)::ls -> typeauto ctx ls (TyAll(tyX, tyS, ty, true))

let rec infer ctx tyS tyT tbl =
  if tyeqv ctx tyS tyT then tbl else
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS, tyT) with
  | (_, TyVar(i, _)) ->
      (* pr ("[debug] S=" ^ formatty ctx tyS ^ ", T=" ^ formatty ctx tyT ^ "\n"); *)
      (* pr ("[debug] var: " ^ string_of_int i ^ "\n"); *)
      List.map (fun (j, x, ty) -> if (=) i j then (j, "", meet ctx ty tyS) else (j, x, ty)) tbl
  | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) ->
      infer ctx tyS2 tyT2 (infer ctx tyT1 tyS1 tbl)
  | (TyAll(tyX1, tyS1, tyS2, _), TyAll(_, tyT1, tyT2, _)) ->
      if not (tyeqv ctx tyS1 tyT1) then raise Not_found;
      let ctx' = addbinding ctx tyX1 (TyVarBind(tyT1)) in
      tblShift (-1) (infer ctx' tyS2 tyT2 (tblShift 1 tbl))
  | (_, _) ->
      (* pr ("[debug] S=" ^ formatty ctx tyS ^ ", T=" ^ formatty ctx tyT ^ "\n"); *)
      raise Not_found

let rec typeapp fi ctx tyS tyT tbl =
  match tyS with
  | TyArr(tyS1, tyS2) ->
      let tyT' = typeShift (List.length tbl) tyT in
      let tbl' = infer ctx tyT' tyS1 tbl in
      let tyS1' = substTbl tyS1 tbl' in
      if not (subtype ctx tyT' tyS1') then error fi ("parameter type mismatch, expected " ^ formatty ctx tyS1 ^ ", received " ^ formatty ctx tyT');
      typeauto ctx tbl' (substTbl tyS2 tbl')
  | TyAll(tyX1, tyS1, tyS2, _) ->
      let binding = TyVarBind tyS1 in
      let ctx' = addbinding ctx tyX1 binding in
      let tbl' = addToTbl tbl tyX1 tyS1 in
      typeapp fi ctx' tyS2 tyT tbl'
  | _ -> error fi ("expected arrow type, received " ^ formatty ctx tyS)

let rec typeof ctx t =
  match t with
  | TmVar(fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs(fi, x, tyT1, t2) ->
      checkkindstar fi ctx tyT1;
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      typeapp fi ctx (lcst ctx tyT1) tyT2 []
  | TmAscribe(fi, t1, tyT) ->
      checkkindstar fi ctx tyT;
      let tyT1 = typeof ctx t1 in
      if subtype ctx tyT1 tyT then tyT
      else error fi ("body of as-term does not have the expected type " ^ formatty ctx tyT ^ ", received " ^ formatty ctx tyT1)
  | TmTAbs(fi, tyX, tyT1, t2) ->
      let ctx = addbinding ctx tyX (TyVarBind(tyT1)) in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX, tyT1, tyT2, false)
  | TmTApp(fi, t1, tyT2) ->
      let tyT1 = typeof ctx t1 in
      (match lcst ctx tyT1 with
         | TyAll(_, tyT11, tyT12, _) ->
             if not(subtype ctx tyT2 tyT11) then
                  error fi ("type parameter type mismatch, expected " ^ formatty ctx tyT11 ^ ", received " ^ formatty ctx tyT2);
             typeSubstTop tyT2 tyT12
         | _ -> error fi ("universal type expected, received " ^ formatty ctx tyT1))
  | TmString _ -> TyString
  | TmRecord(fi, fields) ->
      let fieldtys = 
        List.map (fun (li, ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match lcst ctx (typeof ctx t1) with
        | TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label " ^ l ^ " not found"))
        | _ -> error fi "Expected record type")
  | TmPack(fi, tyT1, t2, tyT) ->
      checkkindstar fi ctx tyT;
      (match simplifyty ctx tyT with
        | TySome(tyY, tyBound, tyT2) ->
            if not (subtype ctx tyT1 tyBound) then
              error fi "hidden type not a subtype of bound";
            let tyU = typeof ctx t2 in
            let tyU' = typeSubstTop tyT1 tyT2 in
            if subtype ctx tyU tyU' then tyT
            else error fi "doesn't match declared type"
        | _ -> error fi "existential type expected")
  | TmUnpack(fi, tyX, x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      (match lcst ctx tyT1 with
        | TySome(tyY, tyBound, tyT11) ->
            let ctx' = addbinding ctx tyX (TyVarBind tyBound) in
            let ctx'' = addbinding ctx' x (VarBind tyT11) in
            let tyT2 = typeof ctx'' t2 in
            typeShift (-2) tyT2
        | _ -> error fi ("existential type expected, received " ^ formatty ctx tyT1))
  | TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi, t1, t2, t3) ->
      if subtype ctx (typeof ctx t1) TyBool then
        join ctx (typeof ctx t2) (typeof ctx t3)
      else error fi "guard of conditional not a boolean"
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi, t1) ->
      if subtype ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmUnit(fi) -> TyUnit
  | TmFloat _ -> TyFloat
  | TmTimesfloat(fi, t1, t2) ->
      if subtype ctx (typeof ctx t1) TyFloat
      && subtype ctx (typeof ctx t2) TyFloat then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmLet(fi, x, t1, t2) ->
     let tyT1 = typeof ctx t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in         
     typeShift (-1) (typeof ctx' t2)
  | TmInert(fi, tyT) ->
      tyT
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match lcst ctx tyT1 with
         | TyArr(tyT11, tyT12) ->
             if subtype ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi ("arrow type expected, received " ^ formatty ctx tyT1))

let evalbinding ctx b = match b with
  | TmAbbBind(t, tyT) ->
      let t' = eval ctx t in 
      TmAbbBind(t', tyT)
  | bind -> bind
