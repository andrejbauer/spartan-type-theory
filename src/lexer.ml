open Parser

let reserved = [
  ("and", AND) ;
  ("begin", BEGIN) ;
  ("bool", BOOL) ;
  ("case", CASE) ;
  ("command", COMMAND) ;
  ("do", DO) ;
  ("else", ELSE) ;
  ("end", END) ;
  ("external", EXTERNAL) ;
  ("false", BOOLEAN false) ;
  ("function", FUNCTION) ;
  ("if", IF) ;
  ("in", IN) ;
  ("int", INT) ;
  ("let", LET) ;
  ("lim", LIM) ;
  ("load", LOAD) ;
  ("real", REAL) ;
  ("precision", PRECISION) ;
  ("skip", SKIP) ;
  ("then", THEN) ;
  ("trace", TRACE) ;
  ("true", BOOLEAN true) ;
  ("var", VAR) ;
  ("while", WHILE)
]

let name =
  [%sedlex.regexp? (('_' | alphabetic),
                 Star ('_' | alphabetic
                      | 185 | 178 | 179 | 8304 .. 8351 (* sub-/super-scripts *)
                      | '0'..'9' | '\'')) | math]

let digit = [%sedlex.regexp? '0'..'9']
let numeral = [%sedlex.regexp? Opt '-', Plus digit]

let hexdigit = [%sedlex.regexp? ('0'..'9' | 'a'..'f' | 'A'..'F')]
let float = [%sedlex.regexp? Opt '-', Opt ("0x" | "0X" | "0b" | "0B"), Plus hexdigit, '.', Star hexdigit]

let symbolchar = [%sedlex.regexp?  ('!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~')]

let prefixop = [%sedlex.regexp? ('~' | '?' | '!'), Star symbolchar ]
let infixop0 = [%sedlex.regexp? ('=' | '<' | '>' | '|' | '&' | '$'), Star symbolchar]
let infixop1 = [%sedlex.regexp? ('@' | '^'), Star symbolchar ]
let infixop2 = [%sedlex.regexp? ('+' | '-'), Star symbolchar ]
let infixop3 = [%sedlex.regexp? ('*' | '/' | '%'), Star symbolchar ]
let infixop4 = [%sedlex.regexp? "**", Star symbolchar ]

let start_longcomment = [%sedlex.regexp? "(*"]
let end_longcomment= [%sedlex.regexp? "*)"]

let newline = [%sedlex.regexp? ('\n' | '\r' | "\n\r" | "\r\n")]
let hspace  = [%sedlex.regexp? (' ' | '\t' | '\r')]

let quoted_string = [%sedlex.regexp? '"', Star (Compl '"'), '"']

let update_eoi ({ Ulexbuf.pos_end; line_limit;_ } as lexbuf) =
  match line_limit with None -> () | Some line_limit ->
    if pos_end.Lexing.pos_lnum >= line_limit
    then Ulexbuf.reached_end_of_input lexbuf

let loc_of lex = Location.make lex.Ulexbuf.pos_start lex.Ulexbuf.pos_end

let safe_int_of_string lexbuf =
  let s = Ulexbuf.lexeme lexbuf in
  try
    Mpz.of_string s
  with
    Invalid_argument _ -> Ulexbuf.error ~loc:(loc_of lexbuf) (Ulexbuf.BadNumeral s)

let rec token ({ Ulexbuf.end_of_input;_ } as lexbuf) =
  if end_of_input then EOF else token_aux lexbuf

and token_aux ({ Ulexbuf.stream;_ } as lexbuf) =
  let f () = Ulexbuf.update_pos lexbuf in
  match%sedlex stream with
  | newline                  -> f (); Ulexbuf.new_line lexbuf; token_aux lexbuf
  | start_longcomment        -> f (); comments 0 lexbuf
  | Plus hspace              -> f (); token_aux lexbuf
  | quoted_string            -> f ();
     let s = Ulexbuf.lexeme lexbuf in
     let l = String.length s in
     let n = ref 0 in
     String.iter (fun c -> if c = '\n' then incr n) s;
     Ulexbuf.new_line ~n:!n lexbuf;
     QUOTED_STRING (String.sub s 1 (l - 2))
  | '('                      -> f (); LPAREN
  | ')'                      -> f (); RPAREN
  | '|'                      -> f (); BAR
  | "=>" | 8658 | 10233      -> f (); DARROW
  | "->" | 8594 | 10230      -> f (); ARROW
  | "="                      -> f (); EQ
  | ":="                     -> f (); COLONEQ
  | ','                      -> f (); COMMA
  | ':'                      -> f (); COLON
  | ';'                      -> f (); SEMICOLON
  (* We record the location of operators here because menhir cannot handle %infix and
     mark_location simultaneously, it seems. *)
  | prefixop                 -> f (); PREFIXOP (Ulexbuf.lexeme lexbuf, loc_of lexbuf)
  | infixop0                 -> f (); INFIXOP0 (Ulexbuf.lexeme lexbuf, loc_of lexbuf)
  | infixop1                 -> f (); INFIXOP1 (Ulexbuf.lexeme lexbuf, loc_of lexbuf)
  | infixop2                 -> f (); INFIXOP2 (Ulexbuf.lexeme lexbuf, loc_of lexbuf)
  (* Comes before infixop3 because ** matches the infixop3 pattern too *)
  | infixop4                 -> f (); INFIXOP4 (Ulexbuf.lexeme lexbuf, loc_of lexbuf)
  | infixop3                 -> f (); INFIXOP3 (Ulexbuf.lexeme lexbuf, loc_of lexbuf)

  | eof                      -> f (); EOF
  | name                     -> f ();
    let n = Ulexbuf.lexeme lexbuf in
    begin try List.assoc n reserved
    with Not_found -> NAME n
    end
  | float                    -> f (); let r = Ulexbuf.lexeme lexbuf in FLOAT r
  | numeral                  -> f (); let k = safe_int_of_string lexbuf in NUMERAL k
  | any -> f ();
     let w = Ulexbuf.lexeme lexbuf in
     let loc = loc_of lexbuf in
     Ulexbuf.error ~loc (Ulexbuf.Unexpected w)
  | _ -> assert false

and comments level ({ Ulexbuf.stream;_ } as lexbuf) =
  match%sedlex stream with
  | end_longcomment ->
    if level = 0 then
      begin Ulexbuf.update_pos lexbuf; token lexbuf end
    else
      comments (level-1) lexbuf

  | start_longcomment -> comments (level+1) lexbuf
  | '\n'        -> Ulexbuf.new_line lexbuf; comments level lexbuf
  | eof         -> Ulexbuf.error ~loc:(loc_of lexbuf) Ulexbuf.UnclosedComment
  | any         -> comments level lexbuf
  | _           -> assert false


(** run a menhir parser with a sedlexer on a t *)
(* the type of run is also:  *)
(* (t -> 'a) -> ('a, 'b) MenhirLib.Convert.traditional -> t -> 'b *)
let run
    (lexer : Ulexbuf.t -> 'a)
    (parser : (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b)
    (lexbuf : Ulexbuf.t) : 'b =
  let lexer () =
    let token = lexer lexbuf in
    (token, lexbuf.Ulexbuf.pos_start, lexbuf.Ulexbuf.pos_end) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised parser in
  try
    parser lexer
  with
  | Parser.Error ->
     let w = Ulexbuf.lexeme lexbuf in
     let loc = loc_of lexbuf in
     Ulexbuf.error ~loc (Ulexbuf.Unexpected w)
  | Sedlexing.MalFormed ->
     let loc = loc_of lexbuf in
     Ulexbuf.error ~loc Ulexbuf.MalformedUTF8
  (* | Sedlexing.InvalidCodepoint _ -> *)
  (*    assert false (\* Shouldn't happen with UTF8 *\) *)


let read_file parse fn =
  try
    let fh = open_in fn in
    let lex = Ulexbuf.from_channel ~fn fh in
    try
      let terms = run token parse lex in
      close_in fh;
      terms
    with
    (* Close the file in case of any parsing errors. *)
      Ulexbuf.Error err -> close_in fh; raise (Ulexbuf.Error err)
  with
  (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> raise (Ulexbuf.error ~loc:Location.Nowhere (Ulexbuf.SysError msg))


let read_toplevel parse () =
  let all_white str =
    let n = String.length str in
    let rec fold k =
      k >= n ||
      (str.[k] = ' ' || str.[k] = '\n' || str.[k] = '\t') && fold (k+1)
    in
    fold 0
  in

  let ends_with_backslash_or_empty str =
    let i = String.length str - 1 in
    if i >= 0 && str.[i] = '\\'
    then (true, String.sub str 0 i)
    else (all_white str, str)
  in

  let rec read_more prompt acc =
    print_string prompt ;
    let str = read_line () in
    let more, str = ends_with_backslash_or_empty str in
    let acc = acc ^ "\n" ^ str in
    if more
    then read_more "  " acc
    else acc
  in

  let str = read_more "# " "" in
  let lex = Ulexbuf.from_string (str ^ "\n") in
  run token parse lex

let read_string parse s =
  let lex = Ulexbuf.from_string s in
  run token parse lex
