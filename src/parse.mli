exception Parse_error of string

type value = Abs_eval.expr

type operator 
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

val init_parse :
  op_t list -> bytes -> (bytes -> value) -> parser_cfg
