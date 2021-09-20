(*
    Copyright 2018 Eric J. Deiman

    This file is part of the iflat project.

    iflat is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    iflat is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with iflat.  If not, see <https://www.gnu.org/licenses/>.
*)

let mk_literal t =
  match t with
  | "true" -> Abs_eval.True
  | "false" -> Abs_eval.False
  | _ ->
    begin
      try Abs_eval.Int (int_of_string t) with
      | _ -> Abs_eval.Id t
    end

let let_prefix_fn sep1 sep2 mk_node pc tokens =
  let mk_name_body e =
    match e with
    | Abs_eval.Eq ((Abs_eval.Id n), v) -> n, v
    | _ -> raise (Parse.Parse_error ("in letop, invalid operator where = expected"))
  in
  let rec ands lhs toks (ns, bs) =
    let n', b' = mk_name_body lhs in
    match Stream.peek toks with
    | Some n ->
      if n = sep1
      then
        begin
          pc.Parse.expect sep1 toks ;
          ands (pc.Parse.parse pc 0 toks) toks (n'::ns, b'::bs)
        end
      else if n = sep2 then
        begin
          pc.Parse.expect sep2 toks ;
          mk_node (n'::ns) (b'::bs) (pc.Parse.parse pc 0 toks)
        end
      else
        mk_node (n'::ns) (b'::bs) Abs_eval.Empty
    | None -> mk_node (n'::ns) (b'::bs) Abs_eval.Empty
  in ands (pc.Parse.parse pc 0 tokens) tokens ([], [])

let pc = Parse.init_parse
    [
      Parse.Preinfix ("fn", 0, "->",
                      (fun a b ->
                         match a with
                         | Abs_eval.Id n -> Abs_eval.Fun (n, b)
                         | _ -> raise (Parse.Parse_error
                                         "Id expected on rhs of -> in fn definition")
                      )) ;
      Parse.Delim "->" ;

      Parse.IKWID ("let", 0, 0,
                   (fun _ _ _ ->
                      raise (Parse.Parse_error ("let is not an infix operator"))),
                   let_prefix_fn "and" "in" (fun ns bs e -> Abs_eval.Let (ns, bs, e))
                  );
      Parse.Delim "and" ;
      Parse.Delim "in" ;

      Parse.IKWID ("fix", 0, 0,
                   (fun _ _ _ ->
                      raise (Parse.Parse_error ("fix is not an infix operator"))),
                   let_prefix_fn "and" "in" (fun ns bs e -> Abs_eval.Fix (ns, bs, e))
                  );

      Parse.Tertiary ("if", 10, "then", "else",
                      (fun e1 e2  e3 -> Abs_eval.Cond (e1, e2, e3))) ;
      Parse.Delim "then" ;
      Parse.Delim "else" ;

      Parse.Infix ("||", 14, (fun e1 e2 -> Abs_eval.Or (e1, e2))) ;
      Parse.Infix ("&&", 16, (fun e1 e2 -> Abs_eval.And (e1, e2))) ;

      Parse.Infix ("<",  20, (fun e1 e2 -> Abs_eval.Lss (e1, e2))) ;
      Parse.Infix ("<=", 20, (fun e1 e2 ->
          Abs_eval.Or (Abs_eval.Lss (e1, e2), Abs_eval.Eq (e1, e2)))) ;
      Parse.Infix (">",  20, (fun e1 e2 -> Abs_eval.Gtr (e1, e2))) ;
      Parse.Infix (">=", 20, (fun e1 e2 ->
          Abs_eval.Or (Abs_eval.Gtr (e1, e2), Abs_eval.Eq (e1, e2)))) ;
      Parse.Infix ("!=", 20, (fun e1 e2 -> Abs_eval.Not (Abs_eval.Eq (e1, e2)))) ;
      Parse.Infix ("=",  20, (fun e1 e2 -> Abs_eval.Eq (e1, e2))) ;

      Parse.Prefix ("not", 20, (fun e -> Abs_eval.Not e)) ;

      Parse.Pre_or_infix ("+", 40, 30, (fun x -> x),
                          (fun e1 e2 -> Abs_eval.Plus (e1, e2))) ;
      Parse.Pre_or_infix ("-", 40, 30, (fun x -> Abs_eval.Neg x),
                          (fun e1 e2 -> Abs_eval.Minus (e1, e2))) ;

      Parse.Infix ("*", 40, (fun e1 e2 -> Abs_eval.Times (e1, e2))) ;
      Parse.Infix ("/", 40, (fun e1 e2 -> Abs_eval.Div (e1, e2))) ;
      Parse.Infix ("mod", 40, (fun e1 e2 -> Abs_eval.Mod (e1, e2))) ;

      Parse.Infixr ("**", 50, (fun e1 e2 -> Abs_eval.Exp (e1, e2))) ;

      Parse.Infix ("@@", 60, (fun e1 e2 -> Abs_eval.App (e1, e2))) ;

      Parse.IKWID ("(", 60, 0,
                   (fun pc tokens left ->
                      let right = pc.Parse.parse pc 0 tokens in
                      pc.Parse.expect ")" tokens ;
                      Abs_eval.App (left, right)
                   ),
                   (fun pc tokens ->
                      let right = pc.Parse.parse pc 0 tokens
                      in pc.Parse.expect ")" tokens ; right
                   )
                  ) ;

      Parse.Delim ")" ;
    ]
    "@@"
    mk_literal

let cases =
  [
    ("2 + 3" ,
     Abs_eval.Int 5) ;
    ("-3**2" ,
     Abs_eval.Int (-9)) ;
    ("let x = 2 in let y = 3 in y - x",
     Abs_eval.Int 1) ;
    ("let x = 2 and y = 3 in y - x",
     Abs_eval.Int 1) ;
    ("let x = 2 in if 1 = 1 then let y = 4 in x + y else 7",
     Abs_eval.Int 6) ;
    ("let x = 2 in if 1 = 2 then let y = 4 in x + y else 7",
     Abs_eval.Int 7) ;
    ("(fn a -> a + 3) 7",
     Abs_eval.Int 10) ;
    ("let a = (fn x -> fn y -> x + y + 1) in a 2 3",
     Abs_eval.Int 6) ;
    ("let a = (fn x -> fn y -> x + y + 1) in a 2 (2 + 1)",
     Abs_eval.Int 6) ;
    ("let a = (fn x -> x + 2) in a3",
     Abs_eval.Int 5) ;
    ("(let x = 2 in (fn y -> x + y) ) 3",
     Abs_eval.Int 5) ;
    ("fix even = fn x -> if x = 0 then true else odd (x - 1) and odd = fn x -> if x = 0 " ^
     "then false else even (x - 1) in even 88",
     Abs_eval.True) ;
    ("fix even = fn x -> if x = 0 then true else odd (x - 1) and odd = fn x -> if x = 0 " ^
     "then false else even (x - 1) in even 89",
     Abs_eval.False) ;
    ("fix a = (fn x -> x + 1) and b = (fn x -> x + 2) in a (b 3)",
     Abs_eval.Int 6);
    ("fix A = fn m -> fn n -> if m = 0 then n + 1 else if (m > 0) && (n = 0) then " ^
     "A (m - 1) 1 else A (m - 1) (A m (n - 1) ) in A 3 4",
     Abs_eval.Int 125)
  ]

type answer =
    OK
  | Oops of ((string * Abs_eval.expr) * Abs_eval.expr) list

let do_tests () =
  let status = List.fold_left (fun ((results, success) as a) ((c, r) as d) ->
      try begin
        let res = Abs_eval.eval (pc.Parse.parse pc 0
                                   (Lex.lexer (Lex.source_string_stream c))) [] in
        if r <> res
        then ((d, res)::results, false)
        else a
      end
      with
      | Parse.Parse_error m -> raise (Parse.Parse_error (m ^ " in '" ^ c ^ "'"))
    ) ([], true) cases
  in
  match status with
  | (_, true) -> OK
  | (results, _) -> Oops results

let rec repl ins partial_expr env =
  Printf.printf "%s %!" (if String.length partial_expr = 0 then "?" else "??") ;
  let input = read_line () in
  let lexer = Lex.lexer (Lex.source_string_stream input) in
  match Stream.peek lexer with
  | None -> repl ins "" env
  | _ ->
    try
      begin
        let full_expr = partial_expr ^ " " ^ input in
        let res = pc.Parse.parse pc 0 (Lex.lexer (Lex.source_string_stream full_expr)) in
        match res with
        | Abs_eval.Let (ns, bs, Abs_eval.Empty) ->
          repl ins "" (Abs_eval.mk_extend_env ns bs env)
        | Abs_eval.Fix (ns, bs, Abs_eval.Empty) ->
          repl ins "" (Abs_eval.mk_recursive_env ns bs env)
        | _ ->
          Printf.printf "%s\n%!"
            (match Abs_eval.eval res env with
             | Abs_eval.Int i -> string_of_int i
             | Abs_eval.True -> "tis true"
             | Abs_eval.False -> "tis false"
             | _ -> "WTF") ;
          repl ins "" env
      end
    with
    | Stream.Failure -> repl ins (partial_expr ^ " " ^ input) env

let () =
  Printf.printf "toylang 0.6\n%!" ;
  repl stdin "" []
