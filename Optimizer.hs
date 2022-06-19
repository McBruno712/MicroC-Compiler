-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN

module Optimizer where

import Syntax


-- Implementar
optimize :: Program -> Program
optimize (Program cmpStmts) = Program (eliminateDeadCode (optimizeStmtExprs cmpStmts))
   where         
        optimizeExpr :: Expr -> Expr
        -- neutro a la izquierda
        optimizeExpr (Binary Plus (NatLit 0) expr) = expr
        optimizeExpr (Binary Mult (NatLit 1) expr) = expr
        optimizeExpr (Binary Or (NatLit 0) expr) = expr

        -- neutro a la derecha
        optimizeExpr (Binary Plus expr (NatLit 0)) = expr
        optimizeExpr (Binary Mult expr (NatLit 1)) = expr
        optimizeExpr (Binary Or expr (NatLit 0)) = expr

        -- nulo a la izquierda
        optimizeExpr (Binary Mult (NatLit 0) expr) = NatLit 0
        optimizeExpr (Binary And (NatLit 0) expr) = NatLit 0
        optimizeExpr (Binary Or (NatLit n) expr) = if n /= 0 then NatLit n else (Binary Or (NatLit n) expr)

        -- neutro AND a la izquierda (esta aca para que funque el pattern matching)
        optimizeExpr (Binary And (NatLit n) expr) = if n /= 0 then expr else (Binary And (NatLit n) expr)

        -- nulo a la derecha
        optimizeExpr (Binary Mult expr (NatLit 0)) = NatLit 0
        optimizeExpr (Binary And expr (NatLit 0)) = NatLit 0
        optimizeExpr (Binary Or expr (NatLit n)) = if n /= 0 then NatLit n else (Binary Or (NatLit n) expr)

        -- neutro AND a la derecha (esta aca para que funque el pattern matching)
        optimizeExpr (Binary And expr (NatLit n)) = if n /= 0 then expr else (Binary And (NatLit n) expr)

        -- evaluar operaciones con constantes
        optimizeExpr (Binary Plus (NatLit x) (NatLit y)) = NatLit (x + y)
        optimizeExpr (Binary Minus (NatLit x) (NatLit y)) = NatLit (x - y)
        optimizeExpr (Binary Mult (NatLit x) (NatLit y)) = NatLit (x * y)
        optimizeExpr (Binary Div (NatLit x) (NatLit y)) = NatLit (div x y)
        optimizeExpr (Binary Mod (NatLit x) (NatLit y)) = NatLit (mod x y)

        --casos recursivos
        optimizeExpr (Unary (Not) expr) = Unary (Not) (optimizeExpr expr)
        optimizeExpr (Unary (Neg) expr) = Unary (Neg) (optimizeExpr expr)

        optimizeExpr (Binary bop expr1 expr2) = bopRec bop expr1 expr2
            where
                bopRec :: BOp -> Expr -> Expr -> Expr 
                bopRec bop expr1 expr2
                    = if ((optimizeExpr expr1) /= expr1) || ((optimizeExpr expr2) /= expr2)
                        then (optimizeExpr (Binary (bop) (optimizeExpr expr1) (optimizeExpr expr2)))
                        else (Binary (bop) (optimizeExpr expr1) (optimizeExpr expr2))                        

            
        -- optimizeExpr (Binary Plus expr1 expr2) = optimizeExpr (Binary (Plus) (optimizeExpr expr1) (optimizeExpr expr2))
        -- optimizeExpr (Binary Mult expr1 expr2) = optimizeExpr (Binary (Mult) (optimizeExpr expr1) (optimizeExpr expr2))
        -- optimizeExpr (Binary And expr1 expr2) = optimizeExpr (Binary (And) (optimizeExpr expr1) (optimizeExpr expr2))
        -- optimizeExpr (Binary Or expr1 expr2) = optimizeExpr (Binary (Or) (optimizeExpr expr1) (optimizeExpr expr2))

        optimizeExpr (Assign name expr) = Assign name (optimizeExpr expr)

        --en los demas casos no hacer nada
        optimizeExpr expr = expr

        optimizeStmtExprs :: [CompoundStmt] -> [CompoundStmt]
        optimizeStmtExprs [] = []
        optimizeStmtExprs ((Decl vdef):cmpStmts) = (Decl vdef):(optimizeStmtExprs cmpStmts)
        optimizeStmtExprs ((Com (StmtExpr expr)):cmpStmts) = (Com (StmtExpr (optimizeExpr expr))):(optimizeStmtExprs cmpStmts)
        optimizeStmtExprs ((Com (PutChar expr)):cmpStmts) = (Com (PutChar (optimizeExpr expr))):(optimizeStmtExprs cmpStmts)        
        optimizeStmtExprs (cStmt:cmpStmts) = cStmt:(optimizeStmtExprs cmpStmts) 

        eliminateDeadCode :: [CompoundStmt] -> [CompoundStmt]
        eliminateDeadCode [] = []
        eliminateDeadCode ((Com (If (NatLit n) bodyIf bodyElse)):cmpStmts) = 
            if n /= 0 then eliminateDeadCode ((map (Com) bodyIf) ++ cmpStmts)
                else eliminateDeadCode ((map (Com) bodyElse) ++ cmpStmts)
        eliminateDeadCode ((Com (While (NatLit 0) body)):cmpStmts) = eliminateDeadCode cmpStmts
        eliminateDeadCode (cStmt:cmpStmts) = cStmt:(eliminateDeadCode cmpStmts)
