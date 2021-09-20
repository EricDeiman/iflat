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

let the_op_list =
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

type answer =
    OK
  | Oops of ((string * Parse.value) * Parse.value) list

let tests pc =
  let cases = [
    ("a + 2" ,
     Abs_eval.Plus (Abs_eval.Id "a", Abs_eval.Int 2));

    ("5 - 3" ,
     Abs_eval.Minus (Abs_eval.Int 5, Abs_eval.Int 3));

    ("-3 * 7" ,
     Abs_eval.Times (Abs_eval.Neg (Abs_eval.Int 3), Abs_eval.Int 7));

    ("7 * - 3" ,
     Abs_eval.Times (Abs_eval.Int 7, Abs_eval.Neg (Abs_eval.Int 3)));

    ("2 * 3 + 4" ,
     Abs_eval.Plus (Abs_eval.Times (Abs_eval.Int 2, Abs_eval.Int 3),
                    Abs_eval.Int 4));

    ("2 * (3 + 4)" ,
     Abs_eval.Times (Abs_eval.Int 2,
                     Abs_eval.Plus (Abs_eval.Int 3, Abs_eval.Int 4)));

    ("2 ** 3 ** 4" ,
     Abs_eval.Exp (Abs_eval.Int 2,
                   Abs_eval.Exp (Abs_eval.Int 3, Abs_eval.Int 4)));
    
    ("-3**2" ,
     Abs_eval.Neg (Abs_eval.Exp (Abs_eval.Int 3, Abs_eval.Int 2)));
    
    ("fn a -> a + 3" ,
     Abs_eval.Fun ("a", Abs_eval.Plus (Abs_eval.Id "a", Abs_eval.Int 3)));
    
    ("fn g -> fn h -> g + h" ,
     Abs_eval.Fun ("g",
                   Abs_eval.Fun ("h", Abs_eval.Plus (Abs_eval.Id "g", Abs_eval.Id "h")))) ;
    
    ("(fn a -> a + 3) 7" ,
     Abs_eval.App
       (Abs_eval.Fun ("a", Abs_eval.Plus (Abs_eval.Id "a", Abs_eval.Int 3)),
        Abs_eval.Int 7));
    
    ("let x = 2 in x * 3" ,
     Abs_eval.Let (["x"], [Abs_eval.Int 2],
                   Abs_eval.Times (Abs_eval.Id "x", Abs_eval.Int 3)));
    
    ("let id = fn x -> x in id 3 " ,
     Abs_eval.Let (["id"], [Abs_eval.Fun ("x", Abs_eval.Id "x")],
                   Abs_eval.App (Abs_eval.Id "id", Abs_eval.Int 3)));
    
    ("let x = 2 in let y = 3 in y - x" ,
     Abs_eval.Let (["x"], [Abs_eval.Int 2],
                   Abs_eval.Let (["y"], [Abs_eval.Int 3],
                                 Abs_eval.Minus (Abs_eval.Id "y", Abs_eval.Id "x"))));
    
    ("if 7 then 8 else 9" ,
     Abs_eval.Cond (Abs_eval.Int 7, Abs_eval.Int 8, Abs_eval.Int 9));
    
    ("if 7 then let x = 8 in x else 9 -5" ,
     Abs_eval.Cond (Abs_eval.Int 7,
                    Abs_eval.Let (["x"], [Abs_eval.Int 8], Abs_eval.Id "x"),
                    Abs_eval.Minus (Abs_eval.Int 9, Abs_eval.Int 5)));
    
    ("if 1 then if 2 then 7 else 8 else 3" ,
     Abs_eval.Cond (Abs_eval.Int 1,
                    Abs_eval.Cond (Abs_eval.Int 2, Abs_eval.Int 7, Abs_eval.Int 8),
                    Abs_eval.Int 3));
    
    ("let x = 2 in if 3 then x + x else 2 * x" ,
     Abs_eval.Let (["x"], [Abs_eval.Int 2],
                   Abs_eval.Cond (Abs_eval.Int 3,
                                  Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Id "x"),
                                  Abs_eval.Times (Abs_eval.Int 2, Abs_eval.Id "x"))));
    
    ("let x = 2 in if 3 then fn x -> x else charlie horse" ,
     Abs_eval.Let (["x"], [Abs_eval.Int 2],
                   Abs_eval.Cond (Abs_eval.Int 3, Abs_eval.Fun ("x", Abs_eval.Id "x"),
                                  Abs_eval.App (Abs_eval.Id "charlie", Abs_eval.Id "horse")))) ;
    
    ("let x = 2 in if 3 then let y = 4 in y else 7" ,
     Abs_eval.Let (["x"], [Abs_eval.Int 2],
                   Abs_eval.Cond (Abs_eval.Int 3,
                                  Abs_eval.Let (["y"], [Abs_eval.Int 4], Abs_eval.Id "y"), Abs_eval.Int 7))) ;
    
    ("if x = y then x else y" ,
     Abs_eval.Cond (Abs_eval.Eq (Abs_eval.Id "x", Abs_eval.Id "y"),
                    Abs_eval.Id "x", Abs_eval.Id "y")) ;
    
    ("-3-3" , Abs_eval.Minus (Abs_eval.Neg (Abs_eval.Int 3), Abs_eval.Int 3)) ;
    
    ("6/3/2" ,
     Abs_eval.Div (Abs_eval.Div (Abs_eval.Int 6, Abs_eval.Int 3),
                   Abs_eval.Int 2)) ;
    
    ("let x = if 7 = 11 then 7 else 11 in x" ,
     Abs_eval.Let (["x"],
                   [Abs_eval.Cond (Abs_eval.Eq (Abs_eval.Int 7, Abs_eval.Int 11),
                                   Abs_eval.Int 7, Abs_eval.Int 11)],
                   Abs_eval.Id "x")) ;
    
    ("fix a = (fn x -> x + 2) and b = (fn y -> y + 1) in a (b 3)"  ,
     Abs_eval.Fix (["b"; "a"],
                   [Abs_eval.Fun ("y", Abs_eval.Plus (Abs_eval.Id "y", Abs_eval.Int 1));
                    Abs_eval.Fun ("x", Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Int 2))],
                   Abs_eval.App (Abs_eval.Id "a",
                                 Abs_eval.App (Abs_eval.Id "b", Abs_eval.Int 3)))
    ) ;

    ("let a = (fn x -> x + 2) and b = (fn y -> y + 1) in a (b 3)",
     Abs_eval.Let (["b"; "a"],
                   [Abs_eval.Fun ("y", Abs_eval.Plus (Abs_eval.Id "y", Abs_eval.Int 1));
                    Abs_eval.Fun ("x", Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Int 2))],
                   Abs_eval.App (Abs_eval.Id "a",
                                 Abs_eval.App (Abs_eval.Id "b", Abs_eval.Int 3)))) ;

    ("(let x = 2 in (fn y -> x + y) ) 3",
     Abs_eval.App
       (Abs_eval.Let (["x"], [Abs_eval.Int 2],
                      Abs_eval.Fun ("y", Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Id "y"))),
        Abs_eval.Int 3)) ;

    ("let id = (fn x -> x)",
     Abs_eval.Let (["id"], [Abs_eval.Fun ("x", Abs_eval.Id "x")],
                   Abs_eval.Empty)) ;

    (" let a = (fn x -> fn y -> x + y + 1) in a 2 3",
     Abs_eval.Let (["a"],
                   [Abs_eval.Fun ("x",
                                  Abs_eval.Fun ("y",
                                                Abs_eval.Plus (Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Id "y"),
                                                               Abs_eval.Int 1)))],
                   Abs_eval.App (Abs_eval.App (Abs_eval.Id "a", Abs_eval.Int 2),
                                 Abs_eval.Int 3))) ;

    ("let a = (fn x -> fn y -> x + y + 1) in a 2 (2 + 1)",
     Abs_eval.Let (["a"],
                   [Abs_eval.Fun ("x",
                                  Abs_eval.Fun ("y",
                                                Abs_eval.Plus (Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Id "y"),
                                                               Abs_eval.Int 1)))],
                   Abs_eval.App (Abs_eval.App (Abs_eval.Id "a", Abs_eval.Int 2),
                                 Abs_eval.Plus (Abs_eval.Int 2, Abs_eval.Int 1)))) ;

    ("let a = (fn x -> fn y -> x + y + 1) in a @@ 2 @@ (2 + 1)",
     Abs_eval.Let (["a"],
                   [Abs_eval.Fun ("x",
                                  Abs_eval.Fun ("y",
                                                Abs_eval.Plus (Abs_eval.Plus (Abs_eval.Id "x", Abs_eval.Id "y"),
                                                               Abs_eval.Int 1)))],
                   Abs_eval.App (Abs_eval.App (Abs_eval.Id "a", Abs_eval.Int 2),
                                 Abs_eval.Plus (Abs_eval.Int 2, Abs_eval.Int 1)))) ;

    ("3 - 4 - 5",
     Abs_eval.Minus (Abs_eval.Minus (Abs_eval.Int 3, Abs_eval.Int 4),
                     Abs_eval.Int 5)) ;

    ("a && b || c && not d",
     Abs_eval.Or (Abs_eval.And (Abs_eval.Id "a", Abs_eval.Id "b"),
                  Abs_eval.And (Abs_eval.Id "c", Abs_eval.Not (Abs_eval.Id "d"))))

  ] in
  let status = List.fold_left (fun ((results, success) as a) ((c, r) as d) ->
      try begin
        let res = pc.Parse.parse pc 0 (Lex.lexer (Lex.source_string_stream c)) in
        if r <> res
        then ((d, res)::results, false)
        else a
      end
      with | Parse.Parse_error m -> raise (Parse.Parse_error (m ^ " in '" ^ c ^ "'"))
    ) ([], true) cases
  in
  match status with
  | (_, true) -> OK
  | (results, _) -> Oops results

let parse_test tests =
  let pc = Parse.init_parse the_op_list "@@" mk_literal in
  List.map (fun t -> t, pc.Parse.parse pc 0 (Lex.lexer (Lex.source_string_stream t)))
    tests

let do_tests () =
  tests (Parse.init_parse the_op_list "@@" mk_literal)

let () = ignore (
    match do_tests () with
    | OK -> Printf.printf "everything worked\n%!"
    | Oops _ -> Printf.printf "something went wrong\n%!"
  )
