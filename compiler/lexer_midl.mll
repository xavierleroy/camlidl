(* Lexer for IDL interface files *)

{
open Utils
open Parser_midl

let keywords = Hashtbl.create 29

let _ =
  List.iter
    (fun (txt, kwd) -> Hashtbl.add keywords txt kwd)
    [ "boolean", BOOLEAN;
      "byte", BYTE;
      "case", CASE;
      "char", CHAR;
      "const", CONST;
      "cpp_quote", CPP_QUOTE;
      "default", DEFAULT;
      "double", DOUBLE;
      "enum", ENUM;
      "false", FALSE;
      "float", FLOAT;
      "handle_t", HANDLE_T;
      "hyper", HYPER;
      "import", IMPORT;
      "int", INT;
      "interface", INTERFACE;
      "long", LONG;
      "NULL", NULL;
      "short", SHORT;
      "signed", SIGNED;
      "sizeof", SIZEOF;
      "small", SMALL;
      "struct", STRUCT;
      "switch", SWITCH;
      "true", TRUE;
      "typedef", TYPEDEF;
      "union", UNION;
      "unsigned", UNSIGNED;
      "void", VOID;
      "wchar_t", WCHAR_T ]

let string_buffer = Ebuff.create 80
let diversion_buffer = Ebuff.create 256
let uuid_buffer = Ebuff.create 16

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

(* To report lexical errors *)

exception Lex_error of string

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
      { Ebuff.reset string_buffer;
        string lexbuf;
        STRING(Ebuff.get_stored string_buffer) }
  | "'" [^ '\\' '\''] "'"
      { CHARACTER(Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { CHARACTER(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' ['0'-'3'] ['0'-'7']? ['0'-'7']? "'"
      { CHARACTER(char_for_code lexbuf 2 1) }
  | "{" ['a'-'z''A'-'Z']* "|"
      { let s = Lexing.lexeme lexbuf in
        let kind = String.lowercase (String.sub s 1 (String.length s - 2)) in
        Ebuff.reset diversion_buffer;
        diversion lexbuf;
        DIVERSION(kind, Ebuff.get_stored diversion_buffer) }
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
  | "-" { MINUS }
  | "?" { QUESTIONMARK }
  | '(' hex8 '-' hex4 '-' hex4 '-' hex4 '-' hex12 ')'
        { Ebuff.reset uuid_buffer;
          scan_uuid (Lexing.from_string (Lexing.lexeme lexbuf));
          UUID(Ebuff.get_stored uuid_buffer) }
  | eof { EOF }
  | _   { raise (Lex_error ("Illegal character " ^
                            Char.escaped(Lexing.lexeme_char lexbuf 0))) }

and comment = parse
    "*/" { token lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lex_error "Unterminated comment") }

and string = parse
    '"'
      { () }
  | '\\' '\n' [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { Ebuff.add_char string_buffer
                       (char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'3'] ['0'-'7']? ['0'-'7']? 
      { Ebuff.add_char string_buffer (char_for_code lexbuf 1 0);
         string lexbuf }
  | eof
      { raise (Lex_error "Unterminated string") }
  | _
      { Ebuff.add_char string_buffer (Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and diversion = parse
    "|}"
      { () }
  (* TODO: skip strings correctly *)
  | eof
      { raise (Lex_error "Unterminated {| section") }
  | _
      { Ebuff.add_char diversion_buffer (Lexing.lexeme_char lexbuf 0);
        diversion lexbuf }

and scan_uuid = parse
    eof
      { () }
  | hex hex
      { let n = int_of_string ("0x" ^ Lexing.lexeme lexbuf) in
        Ebuff.add_char uuid_buffer (Char.chr n);
        scan_uuid lexbuf }
  | _
      { scan_uuid lexbuf }
