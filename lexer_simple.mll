(* Lexer for IDL interface files *)

{
open Utils
open Parser_simple

let keywords = Hashtbl.create 29

let _ =
  List.iter
    (fun (txt, kwd) -> Hashtbl.add keywords txt kwd)
    [ "boolean", BOOLEAN;
      "byte", BYTE;
      "case", CASE;
      "char", CHAR;
      "default", DEFAULT;
      "double", DOUBLE;
      "enum", ENUM;
      "false", FALSE;
      "float", FLOAT;
      "int", INT;
      "interface", INTERFACE;
      "long", LONG;
      "NULL", NULL;
      "short", SHORT;
      "small", SMALL;
      "struct", STRUCT;
      "true", TRUE;
      "typedef", TYPEDEF;
      "union", UNION;
      "unsigned", UNSIGNED;
      "void", VOID ]

(* To buffer string literals *)

type buffer =
  { initial: string;
    mutable buff: string;
    mutable index: int }

let new_buffer sz =
  let s = String.create sz in { initial = s; buff = s; index = 0 }

let reset b =
  b.buff <- b.initial; b.index <- 0

let store b c =
  if b.index >= String.length b.buff then begin
    let new_buff = String.create (String.length b.buff * 2) in
    String.blit b.buff 0 new_buff 0 (String.length b.buff);
    b.buff <- new_buff
  end;
  String.unsafe_set b.buff b.index c;
  b.index <- b.index + 1

let get_stored b =
  let s = String.sub b.buff 0 b.index in
  b.buff <- b.initial;
  s

let string_buffer = new_buffer 80
let diversion_buffer = new_buffer 256
let uuid_buffer = new_buffer 16

(* To translate escape sequences *)

let char_for_backslash = function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c

let char_for_code lexbuf i j =
  let s = Lexing.lexeme lexbuf in
  let s' = String.sub s i (String.length s - i - j) in
  Char.chr(int_of_string("0o" ^ s'))
}

let blank = [' ' '\010' '\013' '\009' '\012']
let identstart = ['A'-'Z' 'a'-'z' '_']
let identchar = identstart | ['0'-'9']
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let octal_literal = '0' ['0'-'7']+
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let hex4 = hex hex hex hex
let hex8 = hex4 hex4
let hex12 = hex4 hex4 hex4

rule token = parse
    blank +
      { token lexbuf }
  | "/*"
      { comment lexbuf }
  | "//" [ ^ '\n' ] * '\n'
      { token lexbuf }
  | identstart identchar *
      { let s = Lexing.lexeme lexbuf in
        try
          Hashtbl.find keywords s
        with Not_found ->
          IDENT s }
  | octal_literal
      { INTEGER(int_of_string("0o" ^ Lexing.lexeme lexbuf)) }
  | decimal_literal | hex_literal
      { INTEGER(int_of_string(Lexing.lexeme lexbuf)) }
  | "\""
      { reset string_buffer;
        string lexbuf;
        STRING(get_stored string_buffer) }
  | "'" [^ '\\' '\''] "'"
      { CHARACTER(Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { CHARACTER(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' ['0'-'3'] ['0'-'7']? ['0'-'7']? "'"
      { CHARACTER(char_for_code lexbuf 2 1) }
  | "{" ['a'-'z''A'-'Z']* "|"
      { let s = Lexing.lexeme lexbuf in
        let kind = String.lowercase (String.sub s 1 (String.length s - 2)) in
        reset diversion_buffer;
        diversion lexbuf;
        DIVERSION(kind, get_stored diversion_buffer) }
  | "#" [' ' '\t']* ['0'-'9']+ [' ' '\t']* "\"" [^ '\n' '\r'] *
    ('\n' | '\r' | "\r\n")
      (* # linenum "filename" flags \n *)
      { token lexbuf }
  | "&" { AMPER }
  | "&&" { AMPERAMPER }
  | "!" { BANG }
  | "!=" { BANGEQUAL }
  | "|" { BAR }
  | "||" { BARBAR }
  | "^" { CARET }
  | ":" { COLON }
  | "," { COMMA }
  | "." { DOT }
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | ">" { GREATER }
  | ">=" { GREATEREQUAL }
  | ">>" { GREATERGREATER }
  | "{" { LBRACE }
  | "[" { LBRACKET }
  | "<" { LESS }
  | "<=" { LESSEQUAL }
  | "<<" { LESSLESS }
  | "(" { LPAREN }
  | "%" { PERCENT }
  | "+" { PLUS }
  | "}" { RBRACE }
  | "]" { RBRACKET }
  | ")" { RPAREN }
  | ";" { SEMI }
  | "/" { SLASH }
  | "*" { STAR }
  | "~" { TILDE }
  | '(' hex8 '-' hex4 '-' hex4 '-' hex4 '-' hex12 ')'
        { reset uuid_buffer;
          scan_uuid (Lexing.from_string (Lexing.lexeme lexbuf));
          UUID(get_stored uuid_buffer) }
  | eof { EOF }

and comment = parse
    "*/" { token lexbuf }
  | _    { comment lexbuf }
  | eof  { error "Unterminated comment" }

and string = parse
    '"'
      { () }
  | '\\' '\n' [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store string_buffer (char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'3'] ['0'-'7']? ['0'-'7']? 
      { store string_buffer (char_for_code lexbuf 1 0);
         string lexbuf }
  | eof
      { error "Unterminated string" }
  | _
      { store string_buffer (Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and diversion = parse
    "|}"
      { () }
  (* TODO: skip strings correctly *)
  | eof
      { error "Unterminated {| section" }
  | _
      { store diversion_buffer (Lexing.lexeme_char lexbuf 0);
        diversion lexbuf }

and scan_uuid = parse
    eof
      { () }
  | hex hex
      { let n = int_of_string ("0x" ^ Lexing.lexeme lexbuf) in
        store uuid_buffer (Char.chr n);
        scan_uuid lexbuf }
  | _
      { scan_uuid lexbuf }
