(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type ty =
    TyArr of ty * ty
  | TyBool
  | TyNat

type term =
  TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

  | TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
 

type binding =
    NameBind
  | VarBind of ty

type command =
  | Eval of info * term
  | Bind of info * string * binding

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty


(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printty: ty -> unit
val prbinding : context -> binding -> unit
val prctx : context -> unit 

(* Misc *)
val tmInfo: term -> info

