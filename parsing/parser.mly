(* Indentifier *)
%token <string> ID

(* Keywords *)
%token USE
%token IF
%token ELSE
%token WHILE
%token RETURN

(* Literals *)
%token <char> CHAR
%token <string> STRING
%token <int> INT
%token <bool> BOOL

(* Open/close delimeters *)
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE

(* Assignment operator, [=] *)
%token GETS

(* Unary operators *)
%token NOT

(* Binary operators *)
%token MULT
%token HIGHMULT
%token DIV
%token REM
%token PLUS
%token MINUS
%token LT
%token LEQ
%token GEQ
%token GT
%token EQ
%token NEQ

(* Logical Operators *)
%token LAND
%token LOR

(* Other punctuation *)
%token COLON
%token SEMICOLON
%token COMMA

(* End of file *)
%token EOF

(* A primitive type *)
%token <Type.primitive> TYPE

%start <Ast.expr> prog

%%

prog:
  | EOF { () }
