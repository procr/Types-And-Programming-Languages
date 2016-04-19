open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type term =
  TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

  | TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term
  

type binding =
    NameBind 

type context = (string * binding) list

type command =
  | Eval of info * term
  | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

(* ↑dc (t) *)
 let termShift d t =
    let rec walk c t = match t with
        TmVar(fi,x,n) -> if x>=c then TmVar(fi,x+d,n+d) else TmVar(fi,x,n+d)
      | TmAbs(fi,x,t1) -> TmAbs(fi, x, walk (c+1) t1)
      | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2) 
      | t -> t
    in walk 0 t


(* ---------------------------------------------------------------------- *)
(* Substitution *)

(* [j -> s] t, replace j with s (value) in t (abstract) *)
let termSubst j s t =
    let rec walk c t = match t with
        TmVar(fi,x,n) -> if x=j+c then termShift c s else TmVar(fi,x,n)
      | TmAbs(fi,x,t1) -> TmAbs(fi, x, walk (c+1) t1)
      | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2) 
      | t -> t
    in walk 0 t

(*bring s (value) into t (abstract) *)
let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
   TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi

  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_) -> fi
  | TmApp(fi, _, _) -> fi 

(* ---------------------------------------------------------------------- *)
(* Printing *)

let rec printtm ctx t = match t with

   TmIf(fi, t1, t2, t3) ->
       pr "if ";
       printtm ctx t1;
       print_space();
       pr "then ";
       printtm ctx t2;
       print_space();
       pr "else ";
       printtm ctx t3;

  | TmPred(_,t1) ->
       pr "pred "; printtm ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm ctx t1
 
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm ctx t1; pr ")")
     in f 1 t1

  | TmAbs(fi,x,t1) ->
      let (ctx',x') = pickfreshname ctx x in
          pr "(λ"; pr x'; pr ". "; printtm ctx' t1; pr ")"

  | TmApp(fi, t1, t2) ->
      pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"

  | TmVar(fi,x,n) ->
      if ctxlength ctx = n then
          pr (index2name fi ctx x)
      else
          pr "[bad index]"

  | t -> printtm ctx t


let rec prctx ctx = match ctx with
[] -> ()
  | (y,_)::rest ->
          prctx rest;
          pr y;
          pr ", "

let prbinding ctx b = match b with
NameBind -> 
    prctx ctx
  | b -> () 
