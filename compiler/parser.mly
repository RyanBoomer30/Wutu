%{
open Exprs

let full_span() = SourceLoc (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
let tok_span(start, endtok) = SourceLoc (Parsing.rhs_start_pos start, Parsing.rhs_end_pos endtok)

%}

%token <int64> NUM
%token <string> ID
%token DEF ANDDEF ADD1 SUB1 LPARENSPACE LPARENNOSPACE RPAREN LBRACK RBRACK LET IN EQUAL COMMA PLUS MINUS TIMES IF COLON ELSECOLON EOF PRINT PRINTSTACK TRUE FALSE ISBOOL ISNUM ISTUPLE EQEQ LESSSPACE GREATER LESSEQ GREATEREQ AND OR NOT COLONEQ SEMI NIL LAMBDA BEGIN END SHADOW REC UNDERSCORE

%right SEMI
%left COLON COLONEQ
%left PLUS MINUS TIMES GREATER LESSSPACE GREATEREQ LESSEQ EQEQ AND OR
%left LPARENNOSPACE


%type <Exprs.sourcespan Exprs.program> program

%start program

%%

const :
  | NUM { ENumber($1, full_span()) }
  | TRUE { EBool(true, full_span()) }
  | FALSE { EBool(false, full_span()) }
  | NIL %prec SEMI { ENil(full_span()) }

prim1 :
  | ADD1 { Add1 }
  | SUB1 { Sub1 }
  | NOT { Not }
  // | PRINT { Print }
  | ISBOOL { IsBool }
  | ISNUM { IsNum }
  | ISTUPLE { IsTuple }
  | PRINTSTACK { PrintStack }

bindings :
  | bind EQUAL expr { [($1, $3, full_span())] }
  | bind EQUAL expr COMMA bindings { ($1, $3, tok_span(1, 3))::$5 }

namebindings :
  | namebind EQUAL expr { [($1, $3, full_span())] }
  | namebind EQUAL expr COMMA namebindings { ($1, $3, tok_span(1, 3))::$5 }

expr :
  | LET bindings IN expr { ELet($2, $4, full_span()) }
  | LET REC namebindings IN expr { ELetRec($3, $5, full_span()) }
  | IF expr COLON expr ELSECOLON expr { EIf($2, $4, $6, full_span()) }
  | BEGIN expr END { $2 }
  | binop_expr SEMI expr { ESeq($1, $3, full_span()) }
  | binop_expr { $1 }

exprs :
  | expr { [$1] }
  | expr COMMA exprs { $1::$3 }

tuple_expr :
  | LPARENNOSPACE RPAREN { ETuple([], full_span()) }
  | LPARENSPACE RPAREN { ETuple([], full_span()) }
  | LPARENNOSPACE expr COMMA RPAREN { ETuple([$2], full_span()) }
  | LPARENSPACE expr COMMA RPAREN { ETuple([$2], full_span()) }
  | LPARENNOSPACE expr COMMA exprs RPAREN { ETuple($2::$4, full_span()) }
  | LPARENSPACE expr COMMA exprs RPAREN { ETuple($2::$4, full_span()) }

id :
  | ID %prec COLON { EId($1, full_span()) }


prim2 :
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | AND { And }
  | OR { Or }
  | GREATER { Greater }
  | GREATEREQ { GreaterEq }
  | LESSSPACE { Less }
  | LESSEQ { LessEq }
  | EQEQ { Eq }

binop_expr :
  | binop_expr prim2 binop_operand %prec PLUS { EPrim2($2, $1, $3, full_span()) }
  | binop_operand COLONEQ binop_expr %prec COLONEQ {
      match $1 with
      | EGetItem(lhs, idx, _) -> ESetItem(lhs, idx, $3, full_span())
      | _ -> raise Parsing.Parse_error
    }
  | binop_operand %prec PLUS { $1 } 

binop_operand :
  // Primops
  | prim1 LPARENNOSPACE expr RPAREN { EPrim1($1, $3, full_span()) }
  // Tuples
  | tuple_expr { $1 }
  | binop_operand LBRACK expr RBRACK { EGetItem($1, $3, full_span()) }
  // Function calls
  | binop_operand LPARENNOSPACE exprs RPAREN %prec LPARENNOSPACE { EApp($1, $3, Unknown, full_span()) }
  | binop_operand LPARENNOSPACE RPAREN %prec LPARENNOSPACE { EApp($1, [], Unknown, full_span()) }
  // Parentheses
  | LPARENSPACE expr RPAREN { $2 }
  | LPARENNOSPACE expr RPAREN { $2 }
  // Lambdas
  | LPARENNOSPACE LAMBDA LPARENNOSPACE binds RPAREN COLON expr RPAREN { ELambda($4, $7, full_span()) }
  | LPARENNOSPACE LAMBDA LPARENSPACE binds RPAREN COLON expr RPAREN { ELambda($4, $7, full_span()) }
  | LPARENNOSPACE LAMBDA COLON expr RPAREN { ELambda([], $4, full_span()) }
  | LPARENSPACE LAMBDA LPARENNOSPACE binds RPAREN COLON expr RPAREN { ELambda($4, $7, full_span()) }
  | LPARENSPACE LAMBDA LPARENSPACE binds RPAREN COLON expr RPAREN { ELambda($4, $7, full_span()) }
  | LPARENSPACE LAMBDA COLON expr RPAREN { ELambda([], $4, full_span()) }
  // Simple cases
  | const { $1 }
  | id { $1 }

decl :
  | DEF ID LPARENNOSPACE RPAREN COLON expr { DFun($2, [], $6, false, full_span()) }
  | DEF ID LPARENNOSPACE binds RPAREN COLON expr { DFun($2, $4, $7, false, full_span()) }
  | DEF SHADOW ID LPARENNOSPACE RPAREN COLON expr { DFun($3, [], $7, true, full_span()) }
  | DEF SHADOW ID LPARENNOSPACE binds RPAREN COLON expr { DFun($3, $5, $8, true, full_span()) }

binds :
  | bind { [$1] }
  | bind COMMA binds { $1::$3 }

bind :
  | namebind { $1 }
  | blankbind { $1 }
  | LPARENNOSPACE binds RPAREN { BTuple($2, full_span()) }
  | LPARENSPACE binds RPAREN { BTuple($2, full_span()) }

blankbind :
  | UNDERSCORE %prec SEMI { BBlank(full_span()) }

namebind :
  | ID %prec SEMI { BName($1, false, full_span()) }
  | SHADOW ID %prec SEMI { BName($2, true, full_span()) }

declgroup :
  | decl { [$1] }
  | decl ANDDEF declgroup { $1::$3 }

decls :
  | { [] }
  | declgroup decls { $1::$2 }


program :
  | decls expr EOF { Program($1, $2, full_span()) }

%%
