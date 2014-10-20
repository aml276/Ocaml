open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

exception WrongWord
exception NilError
exception InvalidLambda
exception InvalidExpressionList
exception RepeatVar
exception InvalidIf
exception InvalidAssignment

let rec make_list input: 'a list =
  match input with 
  | Cons (d1, Nil) -> [d1] 
  | Cons (d1, d2) -> d1::(make_list d2)
  | _ -> [input] 

let rec make_var_list (input: datum): variable list =
  match input with
  | Cons (Atom (Identifier v), Nil) -> if Identifier.is_valid_variable v then [Identifier.variable_of_identifier v] else raise InvalidLambda
  | Cons (Atom (Identifier v), d2) -> if Identifier.is_valid_variable v then (Identifier.variable_of_identifier v)::(make_var_list d2) else raise InvalidLambda
  | _ -> raise InvalidLambda

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id -> ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> raise WrongWord
     (* Above match case didn't succeed, so id is not a valid variable. *)
  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b)
  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i)
  | Nil -> raise NilError
  | Cons (Atom (Identifier id), d2) when Identifier.is_keyword id-> 
      (match Identifier.string_of_identifier id with 
      | "quote" -> ExprQuote d2
      | "lambda" -> (match d2 with
                    | Cons (Cons (d1, d2), d3) -> ExprLambda ((let temp = make_var_list (Cons (d1, d2)) in (if List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] temp = temp then temp else raise RepeatVar)), make_expr_list d3)
                    | _ -> raise InvalidLambda)
      | "if" -> (match d2 with 
                | Cons(a, Cons(b, Cons(c, Nil))) -> ExprIf (read_expression a, read_expression b, read_expression c) 
                | _ -> raise InvalidIf)
      | "set!" -> (match d2 with
                  | Cons(Atom (Identifier a), Cons(b, Nil)) when Identifier.is_valid_variable a-> ExprAssignment (Identifier.variable_of_identifier a, read_expression b)
                  | _ -> raise InvalidAssignment)
      | "let" -> (match d2 with
                 | Cons (Cons (d1, d2), d3) -> ExprLet (let temp = make_list (Cons(d1, d2)) in  ,[])
                 | _ -> )
      |_-> failwith "Sdfsf")
  | _ -> failwith "sdasd"

and make_expr_list (input: datum) : expression list =
  match input with 
  | Cons (d1, Nil) -> [read_expression d1] 
  | Cons (d1, d2) -> (read_expression d1)::(make_expr_list d2)
  | _ -> raise InvalidExpressionList


     

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | _ -> failwith "Sing the Rowing Song!"

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  failwith "You know!"

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating _
  | ExprVariable _        ->
     failwith "'Oh I sure love to row my boat with my...oar."
  | ExprQuote _           ->
     failwith "Rowing!"
  | ExprLambda (_, _)
  | ExprProcCall _        ->
     failwith "Sing along with me as I row my boat!'"
  | ExprIf (_, _, _) ->
     failwith "But I love you!"
  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
