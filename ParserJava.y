
{
module ParserJava where
import Data.Char
import Data.String
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    var                { TokenVar $$ }
    number             { TokenNumber $$ }
    lam                { TokenLam }
    '.'                { TokenDot }
    '('                { TokenOB }
    ')'                { TokenCB }
    '{'                { TokenOC }
    '}'                { TokenCC }
    '='                 { TokenColon}
    '>'                { TokenArrow }
    boole               { TokenBoole }
    "int"               { TokenIntT }
    "char"               { TokenCharT }
    "void"               { TokenVoidT }
    '+'                 {TokenPlus}
    ';'                {TokenDotComma}
    ','                {TokenComma}
	
    "class"              {TokenClass}
    "//"              {TokenBreakClass}
    "extends"              {TokenExt}
    "implements"            {TokenImpl}
    "is"              {TokenIs}
    "return"              {TokenReturn}
    "if"              {TokenIf}
    "then"              {TokenThen}
    "else"              {TokenElse}
    ":=="              {TokenAtrib}
	"this" 				{TokenThis}
	"true" 				{TokenTrue}
	"false" 				{TokenFalse}

%%

-- PEDAÇO DA GRAMÁTICA (INCOMPLETA)
--Program		: '{' ClassBody* '}'
--ClassBody	: ClassId "ext" ClassName '{' MethBody** '}'
--MethBody	: MethId "is" '(' lam ParId ':' VarType ')'** '{' Stmts* ';' "return" Expr '}'
--Stmts		: Stmt ';' Stmts
--			|
--Stmt		: "if" Expr "then" Stmts "else" Stmts
--			| Var ":==" Expr
--			| Expr
--Expr		: Value
--			| Var
--			| Expr '.' MethName(Expr)
--Var			: Name
--			| Var.Name
--			| this
--Value		: PrimValue
--			| RefValue
--PrimValue	: intValue
--			| charValue
--			| byteValue
--RefValue	: Null
--Env			: Env ';' Decl
--			|
--Decl 		: ClassId "ext" ClassName "impl" InterfName '{' (VarId ':' VarType)* (MethId ':' MethType)* '}'
--			| InterfId "ext" InterfName '{' (MethId ':' MethType)* '}'
--			| VarId ':' VarType
--VarType		: PrimType
--			| ClassName
--			| InterfName
--PrimType	: bool | char | int
--MethType 	: ArgType -> (VarType | Void)
--ArgType		: (VarType (xVarType)*)

-- Inicio gramática
Prog 		: '{' ClassList '}'										{ Prog $2 }

-- Lista de Class
ClassList1	: ClassBody ClassList1 									{ $1 : $2 }
			| {- empty -}											{ [] }
ClassList	: ClassList1 											{ ClassList $1 }
ClassBody 	: "class" var ClassExt '{' VarList MethodList '}'		{ ClassBody $2 $3 $5 $6 }

Att 		: T1 var '=' Expr 										{ Att $1 $2 $4 }
VarList		: VarList1												{ $1 }
VarList1	: {- empty -}											{ [] }
			| VarList1 Att ';'										{ $2:$1 }

ClassExt	: "extends" var											{ $2 }
			| {- empty -}											{ "" }
--ClassImpl	: "implements" ClassImplList							{ $2 }
--			| {- empty -}											{ [] }
--ClassImplList: var													{ [$1] }
---			| var ',' ClassImplList									{ $1 : $3 }


-- Lista de MethBody
MethodList1	: MethBody MethodList1 									{ $1 : $2 }
			| {- empty -}											{ [] }
MethodList	: MethodList1 											{ $1 }
--MethBody 	: T var '(' MethParams ')' '{' Stmts "return" Expr ';' '}'	{ MethBody $1 $2 $4 $7 $9 }
MethBody 	: var '(' MethParams ')' '{' Stmts MethReturn'}'  { MethBody (IntType) $1 $3 $6 $7 }

MethReturn	: "return" Expr ';'	{ $2 }
			| {- empty -} 		{ TReturn }

-- Lista de Parameters do Método
MethParams1 : Param ',' MethParams1				{ $1 : $3 }
			| Param								{ [$1] }
			| {- empty -}						{ [] }
MethParams 	: MethParams1						{ $1 }
Param		: T var								{ Param $1 $2 } 

-- Lista de Statements do Método
Stmts1		: Stmt ';' Stmts1					{ $1 : $3 }
			| {- empty -}						{ [] }
Stmts		: Stmts1							{ $1 }
Stmt		: Expr 								{ Stmt $1 } 
			| "if" '(' Value ')' "then" '{' Stmts '}' "else" '{' Stmts '}' { If $3 $7 $11 }
			| Att								{ VarAtt $1 }

Expr		: Var								{ $1 }
			| Value								{ $1 }

Var			: var								{ Expr $1 }
			| "this"							{ This }
			| Var '.' var						{ Acc $1 $3 }
			| Var '.' var '(' MethCallList ')'	{ MethCall $1 $3 $5 }

MethCallList: Expr						{ [$1] }
			| Expr ',' MethCallList		{ $1 : $3 }
			
BoolValue	: "true"							{ TTrue }
			| "false"							{ TFalse }
			
Value		: number							{ Int $1 }
			| BoolValue							{ $1 }

-- Tipos aceitos		 
listT 	 : T1 listT								{ $1 : $2 }
		| {- empty -}							{ [] }
T1 		 : boole                                { Boole }
		| "int"									{ IntType }
		| "char"								{ CharType }
		| "void"								{ VoidType }
T        : listT '>' T                          { FuncType $1 $3 }
        | boole                                 { Boole }
		| "int"									{ IntType }
		| "char"								{ CharType }
		| "void"								{ VoidType }
        | T '+' T                               { SumType $1 $3 }

{

parseError :: [Token] -> a
parseError _ = error "ERRO: Sequencia de caracteres invalida para analise."

-- Definição da sintaxe abstrata na linguagem

data Prog		= Prog ClassList
                deriving (Show, Eq)
data ClassList	= ClassList [CBody]
                deriving (Show, Eq)
data CBody	 	= ClassBody String String [Att] [MBody]
                deriving (Show, Eq)
data MethodList	= MethodList [MBody]
                deriving (Show, Eq)
data MBody		= MethBody T String [Param] [Stmt] Expr
                deriving (Show, Eq)
data MethParams	= MethParams [Param]
                deriving (Show, Eq)
data Param	 	= Param T String
                deriving (Show, Eq)
data Stmt	 	= Stmt Expr
				| VarAtt Att
                | If Expr [Stmt] [Stmt]
                deriving (Show, Eq)
data Att		= Att T String Expr
                deriving (Show, Eq)
data Expr		= Expr String
				| Int Int
				| MethCall Expr String [Expr]
				| This
				| Acc Expr String
				| TTrue
				| TFalse
				| TReturn
                deriving (Show, Eq)

data Env		= Env [Decl]
                deriving (Show, Eq)
				
data Decl 		= ClassDecl String String [(String,T)] [(String,T)]
				| InterfDecl String [String] [(String,T)]
				| VarDecl T String
                deriving (Show, Eq)
				
data T         = Boole
				| IntType
				| CharType
				| VoidType
                | FuncType [T] T
                | SumType T T
                deriving (Show,Eq,Read)

data Token        = TokenVar String
				| TokenNumber Int
                | TokenLam
                | TokenDot
                | TokenOB
                | TokenCB
                | TokenOC
                | TokenCC
                | TokenArrow 
                | TokenBoole
                | TokenIntT
                | TokenCharT
				| TokenVoidT
                | TokenColon
                | TokenPlus
				| TokenProg
				| TokenExt
				| TokenClass
				| TokenBreakClass
				| TokenImpl
				| TokenIs
				| TokenDotComma
				| TokenComma
				| TokenReturn
				| TokenIf
				| TokenThen
				| TokenElse
				| TokenAtrib
				| TokenThis
				| TokenTrue
				| TokenFalse
                deriving (Show)

type TyContext      = [(Char,T)]

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)    | isSpace c = lexer cs
                | isAlpha c = lexStr (c:cs)
				| isDigit c = lexNum (c:cs)
lexer ('.':cs) = TokenDot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOC : lexer cs
lexer ('}':cs) = TokenCC : lexer cs
lexer ('=':cs) = TokenColon : lexer cs --let prox1 = head cs;
--				     prox2 = head (tail cs)
--				 in if (prox1 == '=' && prox2 == '=') then
--				    TokenAtrib : lexer cs
--				 else
--                    TokenColon : lexer cs
lexer ('>':cs) = TokenArrow : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer (';':cs) = TokenDotComma : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexNum cs = TokenNumber (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexStr cs = case span isAlpha cs of
                ("lam", rest)        -> TokenLam : lexer rest
                ("boole", rest)      -> TokenBoole : lexer rest
                ("int", rest)      -> TokenIntT : lexer rest
                ("char", rest)      -> TokenCharT : lexer rest
                ("void", rest)      -> TokenVoidT : lexer rest
                
                ("class", rest)        -> TokenClass : lexer rest
                ("extends", rest)        -> TokenExt : lexer rest
                ("implements", rest)        -> TokenImpl : lexer rest
                ("is", rest)         -> TokenIs : lexer rest
                ("return", rest)     -> TokenReturn : lexer rest
                ("if", rest)         -> TokenIf : lexer rest
                ("then", rest)       -> TokenThen : lexer rest
                ("else", rest)       -> TokenElse : lexer rest
                ("this", rest)       -> TokenThis : lexer rest
                ("true", rest)       -> TokenTrue : lexer rest
                ("false", rest)       -> TokenFalse : lexer rest
				
                (var, rest)          -> TokenVar var : lexer rest
                

lexVar (c:cs) = if cs == [] then 
                  TokenVar c
                else 
				  --TokenVar c
				  --TokenString cs
                  error "ERRO: Variavel definida com mais de um caracter." 


}

