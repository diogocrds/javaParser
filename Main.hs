module Main where

import ParserJava
import Data.Foldable

-- cria tipo da funcao baseada nos parametros e seu retorno
criaFuncType :: [Param] -> T -> [T]
criaFuncType ((Param varT _):cs) funT = [varT]++(criaFuncType cs funT)
criaFuncType [] funT = []

-- cria a lista de declaracoes dos metodos de uma classe
criaDeclMeths :: [MBody] -> [(String,T)]
criaDeclMeths ((MethBody t id params stmts ret):cs) = let 
                                                      funcT = (criaFuncType params t)
													  in (id,(FuncType funcT t)):(criaDeclMeths cs)
criaDeclMeths [] = []

-- cria a lista de declaracoes dos parametros de um metodo
criaDeclParams :: [Param] -> [Decl]
criaDeclParams ((Param t var):cs) = (VarDecl t var):(criaDeclParams cs)
criaDeclParams [] = []

-- verifica se uma variavel esta declarada no environment
verificaVar :: String -> [Decl] -> Bool
verificaVar var ((VarDecl declT decl):cs) = if (var == decl) then True else (verificaVar var cs)
verificaVar var ((ClassDecl _ _ _ _ _):cs) = (verificaVar var cs)
verificaVar var ((InterfDecl _ _ _):cs) = (verificaVar var cs)
verificaVar _ [] = False

-- procura e retorna o tipo de uma variavel no environment
procuraVar :: String -> [Decl] -> T
procuraVar var ((VarDecl declT decl):cs) = if (var == decl) then declT else (procuraVar var cs)
procuraVar var ((ClassDecl _ _ _ _ _):cs) = (procuraVar var cs)
procuraVar var ((InterfDecl _ _ _):cs) = (procuraVar var cs)
procuraVar _ [] = error "ERRO: Variavel nao declarada"

-- ===============================================================
-- retorna tipo de uma Expr
typeOf :: Expr -> [Decl] -> [Decl] -> T
typeOf (Expr var) envG envL = (procuraVar var envL)
typeOf (Int i) envG envL = IntType
typeOf TTrue envG envL = Boole
typeOf TFalse envG envL = Boole

-- ===============================================================
-- verifica o stmt e retorna declaracao se for nova variavel
verificaStmt :: Stmt -> [Decl] -> [Decl] -> [Decl]
verificaStmt (VarA (Att varT var expr)) envG envL = [(VarDecl varT var)]
verificaStmt (Stmt (Expr var)) envG envL = if (verificaVar var envL)==False then
											    error ("ERRO: Variavel "++var++" nao declarada")
										     else []
verificaStmt (Stmt (Int i)) envG envL = []
verificaStmt (If var expr1 expr2) envG envL = let{
                                                   envExpr1 = listaStmt expr1 envG envL;
                                                   envExpr2 = listaStmt expr2 envG envL;
                                                } in 
												if (typeOf var envG envL)==Boole then
												   if ((length envExpr1) >= 0) && ((length envExpr2) >=0) then
												      []
												   else error "ERRO: verifica STMT IF"
												else error "ERRO: condicional do IF nao eh boolean"
verificaStmt (Stmt (This)) envG envL = []
verificaStmt (Stmt (Acc expr var)) envG envL = if (verificaVar var envL)==False then
											    error ("ERRO: Variavel "++var++" nao declarada")
										     else (verificaStmt (Stmt expr) envG envL)
verificaStmt (Stmt (MethCall expr1 name expr2)) envG envL = (verificaStmt (Stmt expr1) envG envL)
veriricaStmt (Stmt (TTrue)) eG eL = []
veriricaStmt (Stmt (TFalse)) eG eL = []
--verificaStmt _ eG eL = error "ERRO: STMT nao aceito / STMT com formato errado"

listaStmt :: [Stmt] -> [Decl] -> [Decl] -> [Decl]
listaStmt (c:cs) envG envL = let {
                                  newStmt = (verificaStmt c envG envL);
								  newEnvL = envL++newStmt;
                                } in (listaStmt cs envG newEnvL)
listaStmt [] envG envL = envL

-- ===============================================================
-- verifica cada stmt do metodo
verificaMethod :: MBody -> [Decl] -> [Decl]
verificaMethod (MethBody t id params stmts ret) env = let
													  declParam = (criaDeclParams params);
                                                      in let
													     vStmts = (listaStmt stmts env declParam);
                                                      in if ((length vStmts) >= 0)then
                                                        env
												      else error "ERRO: problema lista STMT"

listaMethod :: [MBody] -> [Decl] -> [Decl]
listaMethod (c:cs) env = let 
							newEnv = (verificaMethod c env)
						  in (listaMethod cs newEnv)
listaMethod [] env = env

-- ==============================================================
-- recursao inicial para verificar cada classe e criar os environments
verificaClasse :: CBody -> [Decl] -> Decl
verificaClasse (ClassBody name ext imp vars meth) env = let {
                                                      declMeth = (criaDeclMeths meth);
													  vMethods = (listaMethod meth env);
													  vClasse = (elem (ClassDecl name ext imp [] declMeth) env);
												    } in 
													  if ((length vMethods) >= 0) then
													     if vClasse==False then
                                                            (ClassDecl name ext imp [] declMeth)
												         else
												            error ("Classe '"++name++"' ja existente.")
													  else error "ERRO: problema lista METHOD"

listaClasses :: [CBody] -> [Decl] -> [Decl]
listaClasses (c:cs) env = (listaClasses cs (env++[(verificaClasse c env)]))
listaClasses [] env = env

func :: Prog -> [Decl]
func (Prog (ClassList c)) = listaClasses c []

main = do 
        putStr "Input Expression =   "
        readFile "input.txt" >>= print.calc.lexer

--putStr "Inferred Type =   "
--getContents >>= print.typeof [('f',FuncType Boole Boole)] .calc.lexer
--putStrLn " -- "
--putStr "Evaluated Expression =   "
--readFile "input.txt" >>= print.interpret.calc.lexer