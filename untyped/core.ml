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
  | TmAbs(_,_,_,_) -> true
  | _ -> false

(*
 * TmZero
 * TmSucc(_, TmZero),
 * TmSucc(TmSucc(_, TmZero)),
 * TmSucc(TmSucc(TmSucc(_, TmZero)))
 * are values,
 * when 'eval1' apply to them, will get NoRuleApplies finally
 * so 'eval' will return the value itself
 * represente 0, 1 2 3 4 5.... respectively
 * *)


let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')

  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 t1 in
      TmApp(fi, t1', t2)
  | _ -> 
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t



(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
    match t with
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
          let ctx' = addbinding ctx x (VarBind(tyT1)) in
            let tyT2 = typeof ctx' t2 in
                TyArr(tyT1, tyT2)
  | TmApp(fi,t1,t2) ->
          let tyT1 = typeof ctx t1 in
            let tyT2 = typeof ctx t2 in
                (match tyT1 with
                    TyArr(tyT11,tyT12) -> if (=) tyT2 tyT11 then tyT12 else 
                        error fi "parameter type mismatch" 
                    | _ -> error fi "arrow type expected")
   | TmTrue(fi) -> TyBool
   | TmFalse(fi) -> TyBool
   | TmIf(fi,t1,t2,t3) -> 
           if (=) (typeof ctx t1) TyBool then 
               let tyT2 = typeof ctx t2 in 
               if (=) tyT2 (typeof ctx t3) then tyT2 
               else error fi "arms of conditional have different types"
            else error fi "guard of conditional not a boolean"
  | TmSucc(fi,t1) ->
          if (=) (typeof ctx t1) TyNat then TyNat
          else error fi "TmSucc should accept a TyNat"
  | TmPred(fi,t1) ->
          if (=) (typeof ctx t1) TyNat then TyNat
          else error fi "TmPred should accept a TyNat"
  | TmIsZero(fi,t1) ->
          if (=) (typeof ctx t1) TyNat then TyBool
          else error fi "TmIsZero should accept a TyNat"
  | TmZero(_) -> TyNat


