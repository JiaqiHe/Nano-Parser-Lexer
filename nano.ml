exception MLFailure of string

type binop =
  Plus
| Minus
| Mul
| Div
| Eq
| Ne
| Lt
| Le
| And
| Or
| Cons

type expr =
  Const of int
| True
| False
| NilExpr
| Var of string
| Bin of expr * binop * expr
| If  of expr * expr * expr
| Let of string * expr * expr
| App of expr * expr
| Fun of string * expr
| Letrec of string * expr * expr

type value =
  Int of int
| Bool of bool
| Closure of env * string option * string * expr
| Nil
| Pair of value * value

and env = (string * value) list

let binopToString op =
  match op with
      Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v =
  match v with
    Int i ->
      Printf.sprintf "%d" i
  | Bool b ->
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) ->
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) ->
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2)
  | Nil ->
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True ->
        "true"
    | False ->
        "false"
    | Var x ->
        x
    | Bin (e1,op,e2) ->
        Printf.sprintf "%s %s %s"
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) ->
        Printf.sprintf "if %s then %s else %s"
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) ->
        Printf.sprintf "let %s = %s in \n %s"
        x (exprToString e1) (exprToString e2)
    | App (e1,e2) ->
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) ->
        Printf.sprintf "fun %s -> %s" x (exprToString e)
    | Letrec (x,e1,e2) ->
        Printf.sprintf "let rec %s = %s in \n %s"
        x (exprToString e1) (exprToString e2)

(*********************** Some helpers you might need ***********************)

let rec fold f base args =
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) =
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) =
  let temp = listAssoc (x, evn) in
    match temp with
      | None -> raise (MLFailure "Variable not bound")
      | Some v -> v



let rec eval (evn,e) =
  match e with
    | Const a -> Int a
    | True -> Bool true
    | False -> Bool false
    | Var x -> lookup (x, evn)
    | Bin (e1, op, e2) -> if op = Cons then Pair(eval (evn, e1), eval (evn, e2))
                        else
                          let temp1 = eval (evn, e1) in
                          let temp2 = eval (evn, e2) in
                          (match (temp1, temp2) with
                          | (Int a, Int b) ->
                            (match op with
                              | Plus -> Int (a + b)
                              | Minus -> Int (a - b)
                              | Mul -> Int (a * b)
                              | Div -> if b != 0 then Int (a / b) else raise (MLFailure "Denominator is 0")
                              | Eq -> Bool (a = b)
                              | Ne -> Bool (a != b)
                              | Lt -> Bool (a < b)
                              | Le -> Bool (a <= b)
                              | _ -> raise (MLFailure "Undefinded operation 1"))
                          | (Bool a, Bool b) ->
                            (match op with
                              | Eq -> Bool (a = b)
                              | Ne -> Bool (a != b)
                              | And -> Bool (a && b)
                              | Or -> Bool (a || b)
                              | _ -> raise (MLFailure "Undefinded operation 2"))
                          | _ -> raise (MLFailure "Undefined operation 3"))
    | If (e1, e2, e3) -> let temp = eval (evn, e1) in
                          (match temp with
                            | Bool a -> if a then eval (evn, e2)
                                              else eval (evn, e3)
                            | _ -> raise (MLFailure "Undefined operation 4"))
    | Let (x, e1, e2) -> let evn2 = (x, eval (evn, e1)) :: evn in
                          eval(evn2, e2)
    (* | Letrec (x, e1, e2) -> let evn2 = (x, eval (evn, e1)) :: evn in
                                                eval(evn2, e2) *)
    | Letrec (b,e1,e2) -> (let tmp = eval(evn, e1) in
                          match tmp with
                          | Closure (local_evn, f, x, e) ->
                                    eval((b, Closure (local_evn, Some b, x, e))::evn, e2)
                          | _ -> eval((b,tmp)::evn, e2))
    | Fun (x, e) -> Closure(evn, None, x,e)
    | App (e1, e2) -> (match e1 with
                      | Var "hd" ->
                          (match eval(evn,e2) with
                            | Pair (h, t) -> h
                            | _ -> raise (MLFailure "Error"))
                      | Var "tl" ->
                          (match eval(evn,e2) with
                            | Pair (h, t) -> t
                            | _ -> raise (MLFailure "Error"))
                      | _ ->
                        let e11 = eval(evn, e1) in
                        let e21 = eval(evn, e2) in
                        (match e11 with
                          | Closure (nevn, None, x,e) ->
                              eval((x, e21)::(nevn@evn), e)
                          | Closure (nevn, Some k, x,e) ->
                              eval((k, Closure (nevn, None, x, e))::(x,e21)::(nevn@evn), e))
                          | _ -> raise (MLFailure "Undefined function 5"))
    | NilExpr -> Nil
    | _ -> raise (MLFailure "Undefined operation");;

(**********************     Testing Code  ******************************)
