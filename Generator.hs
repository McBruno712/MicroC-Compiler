-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE GENERACIÓN DE CÓDIGO DE MÁQUINA

module Generator where

import Syntax
import MachineLang

-- Implementar
generate :: Program -> Code
generate (Program ((Decl def):cmpStmts)) = [SKIP] ++ (generate (Program cmpStmts))
generate (Program ((Com stmt):cmpStmts)) = (stmt2Code stmt) ++ (generate (Program cmpStmts))
    where
        machineNot  = [JMPZ 3, PUSH 0, JUMP 2, PUSH 1]
        machineAnd  = [MUL]
        machineEqu  = [CMP] ++ machineNot
        machineLess = [CMP, PUSH 1, ADD] ++ machineNot
        machinePop  = [JMPZ 1]

        stmt2Code :: Stmt -> Code
        stmt2Code (StmtExpr expr) = (exp2Code expr) ++ machinePop
        stmt2Code (If expr bodyIf bodyElse) 
            = (exp2Code expr) ++ [JMPZ ((length codeBodyIf)+2)] ++ codeBodyIf
                ++ [JUMP ((length codeBodyEl)+1)] ++ codeBodyEl
            where 
                codeBodyIf = (generate (Program (map (Com) bodyIf)))
                codeBodyEl = (generate (Program (map (Com) bodyElse)))
        stmt2Code (While expr body)
            = (exp2Code expr) ++ [JMPZ ((length codeBody)+2)] ++ codeBody
                ++ [JUMP ((-(length codeBody))-1)]
            where
                codeBody = (generate (Program (map (Com) body)))
        stmt2Code (PutChar expr) = (exp2Code expr) ++ [WRITE]

        exp2Code :: Expr -> Code
        exp2Code (Var name)     = [LOAD name]
        exp2Code (CharLit chr)  = [PUSH (toInteger (fromEnum chr))]
        exp2Code (NatLit n)     = [PUSH n]
        exp2Code (GetChar)      = [READ]
        exp2Code (Unary uop expr) 
            = case uop of
                (Not) -> (exp2Code expr) ++ machineNot
                (Neg) -> (exp2Code expr) ++ [NEG]
        exp2Code (Binary bop expr1 expr2) 
            = case bop of
                (Or)    -> (exp2Code expr1) ++ machineNot ++ (exp2Code expr2) ++ machineNot ++ machineAnd ++ machineNot
                _       -> (exp2Code expr1) ++ (exp2Code expr2) ++ code
                    where 
                        code = case bop of          
                                (And)   -> machineAnd
                                (Equ)   -> machineEqu
                                (Less)  -> machineLess
                                (Plus)  -> [ADD]
                                (Minus) -> [SUB]
                                (Mult)  -> [MUL]
                                (Div)   -> [DIV]
                                (Mod)   -> [MOD]
        exp2Code (Assign name expr) = (exp2Code expr) ++ [STORE name, LOAD name]