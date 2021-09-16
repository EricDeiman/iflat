exception Parse_error of string

type value = Abs_eval.expr

type operator =
  {
    name : string ;
    lbp : int ;
    rbp : int ;
    infix_fn : parser_cfg -> string Stream.t -> value -> value ;
    prefix_fn : parser_cfg -> string Stream.t -> value ;
  }
and parser_cfg =
  {
    op_tbl : (string * operator) list ;
    space_op_sym : string ;
    mk_literal : string -> value ;
    expect : string -> string Stream.t -> unit ;
    parse : parser_cfg -> int -> string Stream.t -> value ;
  }

type op_t =
    Preinfix of (string * int * string *
                 (value -> value -> value))
  | Prefix of (string * int * (value -> value))
  | Tertiary of (string * int * string * string *
                 (value -> value -> value -> value))
  | Infix of (string * int * (value -> value -> value))
  | Pre_or_infix of (string * int * int * (value -> value) *
                     (value -> value -> value))
  | Infixr of (string * int * (value -> value -> value))
    (* I know what I'm doing *)
  | IKWID of (string * int * int *
              (parser_cfg -> string Stream.t -> value -> value) *
              (parser_cfg -> string Stream.t -> value))
  | Delim of string

let assoc_opt n e =
  try Some (List.assoc n e) with
    Not_found -> None

let is_op t op_tbl =
  match assoc_opt t op_tbl with
  | Some _ -> true
  | None -> false

let lbp token op_tbl =
  match assoc_opt token op_tbl with
  | None -> 0
  | Some {lbp = i} -> i

let get_prefix_fn token op_tbl mk_literal =
  match assoc_opt token op_tbl with
  | Some {prefix_fn = f} -> f
  | None -> (fun _ _ -> mk_literal token)

let get_infix_fn token op_tbl =
  match assoc_opt token op_tbl with
  | Some {infix_fn = f} -> f
  | None -> raise (Parse_error (token ^ " is not an infix operator"))

let emptyEnv = []

let infix op_name power mk_node =
  {
    name = op_name ;
    lbp = power ;
    rbp = power ;
    infix_fn = (fun pc tokens left -> mk_node left (pc.parse pc power tokens)) ;
    prefix_fn = (fun _ _ -> raise (Parse_error (op_name ^ "is not a prefix operator"))) ;
  }

let pre_or_infix op_name pre_pow in_pow mk_node_pre mk_node_in =
  {
    name = op_name ;
    lbp = in_pow ;
    rbp = in_pow ;
    infix_fn = (fun pc tokens left -> mk_node_in left (pc.parse pc in_pow tokens)) ;
    prefix_fn = (fun pc tokens -> mk_node_pre (pc.parse pc pre_pow tokens)) ;
  }

let delim op_name =
  {
    name = op_name ;
    lbp = 0 ;
    rbp = 0 ;
    infix_fn = (fun _ _ _ -> raise (Parse_error (op_name ^ " is not an infix operator"))) ;
    prefix_fn = (fun _ _ -> raise (Parse_error (op_name ^ " is not a prefix operator"))) ;
  }

let infixr op_name power mk_node =
  {
    name = op_name ;
    lbp = power ;
    rbp = power - 1 ;
    infix_fn = (fun pc tokens left -> mk_node left (pc.parse pc (power - 1) tokens)) ;
    prefix_fn = (fun _ _ -> raise (Parse_error (op_name ^ " is not a prefix operator"))) ;
  }

let preinfix op_name power sep mk_node =
  {
    name = op_name ;
    lbp = power ;
    rbp = 0 ;
    infix_fn = (fun _ _ _ -> raise (Parse_error (op_name ^ " is not an infix operator"))) ;
    prefix_fn = (fun pc tokens ->
        let arg = pc.parse pc 0 tokens in
        pc.expect sep tokens ;
        mk_node arg (pc.parse pc 0 tokens)
      )
  }

let tertiary op_name power sep1 sep2 mk_node =
  {
    name = op_name ;
    lbp = power ;
    rbp = 0 ;
    infix_fn = (fun _ _ _ -> raise (Parse_error (op_name ^ " is not an infix operator"))) ;
    prefix_fn = (fun pc tokens ->
        let first = pc.parse pc 0 tokens in
        pc.expect sep1 tokens ;
        let second = pc.parse pc 0 tokens in
        pc.expect sep2 tokens ;
        let third = pc.parse pc 0 tokens in
        mk_node first second third
      )
  }

let prefix op_name power mk_node =
  {
    name = op_name ;
    lbp = power ;
    rbp = power ;
    infix_fn = (fun _ _ _ -> raise (Parse_error (op_name ^ " is not an infix operator"))) ;
    prefix_fn = (fun pc tokens -> mk_node (pc.parse pc power tokens)) ;
  }

let ikwid name lbp rbp infix_fn prefix_fn =
  {
    name = name ;
    lbp = lbp ;
    rbp = rbp ;
    infix_fn = infix_fn ;
    prefix_fn = prefix_fn ;
  }

let expect n token_stm =
  match Stream.peek token_stm with
  | None -> raise (Parse_error ("expected " ^ n ^ " but found nothing"))
  | Some t ->
    if n = t
    then ignore (Stream.next token_stm)
    else raise (Parse_error ("expected " ^ n ^ " but found " ^ t))

let rec parse pc rbp token_stm =
  let rec do_infix_fns left =
    let process_op t =
      if rbp < (lbp t pc.op_tbl)
      then
        do_infix_fns ((get_infix_fn (Stream.next token_stm) pc.op_tbl) pc token_stm left)
      else
        left
    in
    let insert_op t =
      if rbp < (lbp t pc.op_tbl)
      then
        do_infix_fns ((get_infix_fn t pc.op_tbl) pc token_stm left)
      else
        left
    in
    match Stream.peek token_stm with
    | None -> left
    | Some t when is_op t pc.op_tbl -> process_op t
    | Some _ -> insert_op pc.space_op_sym
  in
  do_infix_fns ((get_prefix_fn (Stream.next token_stm) pc.op_tbl pc.mk_literal)
                  pc
                  token_stm)

let prepend_op_tbl op_tbl o =
  match o with
  | Preinfix (s1, i, s2, fn) -> (s1, (preinfix s1 i s2 fn))::op_tbl
  | Prefix (s, i, fn) -> (s, (prefix s i fn))::op_tbl
  | Tertiary (s1, i, s2, s3, fn) -> (s1, (tertiary s1 i s2 s3 fn))::op_tbl
  | Infix (s, i, fn) -> (s, (infix s i fn))::op_tbl
  | Pre_or_infix (s, i1, i2, fnp, fni) -> (s,  (pre_or_infix s i1 i2 fnp fni))::op_tbl
  | Infixr (s, i, fn) -> (s, (infixr s i fn))::op_tbl
  | IKWID (s, i1, i2, infix_fn, prefix_fn) ->
    (s, (ikwid s i1 i2 infix_fn prefix_fn))::op_tbl
  | Delim s -> (s,  delim s)::op_tbl

let mk_op_tbl op_list op_tbl = List.fold_left prepend_op_tbl op_tbl op_list

let init_parse op_list space_op_sym mk_literal =
  {
    op_tbl = mk_op_tbl op_list [] ;
    space_op_sym = space_op_sym ;
    mk_literal = mk_literal ;
    expect = expect ;
    parse = parse ;
  }
