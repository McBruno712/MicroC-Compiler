-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TypeChecker where

import Syntax

data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
   show (Duplicated      n)  = "Duplicated definition: " ++ n
   show (Undefined       n)  = "Undefined: " ++ n
   show (Expected    ty ty') = "Expected: " ++ show ty
                             ++ " Actual: " ++ show ty'

type Env = [(Name, Type)]

-- Implementar
checkProgram :: Program -> [Error]
checkProgram = undefined

checkNames :: [Name] -> Program -> [Error]
checkNames _ (Program []) = [] --programa vacio
checkNames declaradas (Program ((Decl (VarDef _ name)):cmpStmts)) --la siguiente instruccion es una declaracion de variable
   = if (elem name declaradas) then ((Duplicated name):(checkNames declaradas (Program cmpStmts)))
      else (checkNames (name:declaradas) (Program cmpStmts))
checkNames declaradas (Program ((Com stmt):cmpStmts)) --la siguiente instruccion es un statement compuesto
   = (stmtPsr declaradas stmt) ++ (checkNames declaradas (Program cmpStmts))
   where 
      stmtPsr :: [Name] -> Stmt -> [Error] --busca errores en el statement
      stmtPsr declaradas stmt
         = case stmt of
            (StmtExpr expr)   -> expPsr declaradas expr
            (If expr bodyIf bodyElse) 
               -> (expPsr declaradas expr) ++ (concat (map (stmtPsr declaradas) bodyIf)) ++ (concat (map (stmtPsr declaradas) bodyElse))
            (While expr body) -> (expPsr declaradas expr) ++ (concat (map (stmtPsr declaradas) body))
            (PutChar expr)    -> expPsr declaradas expr

      expPsr :: [Name] -> Expr -> [Error] --busca errores en la expresion
      expPsr declaradas (Var name)              = if (elem name declaradas) then [] else [Undefined name]
      expPsr declaradas (Unary _ expr)          = expPsr declaradas expr
      expPsr declaradas (Binary _ expr1 expr2)  = (expPsr declaradas expr1) ++ (expPsr declaradas expr2)
      expPsr declaradas (Assign name expr)   
         = if (elem name declaradas) then (expPsr declaradas expr) 
            else ((Undefined name):(expPsr declaradas expr))
                        