-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TypeChecker where

import Syntax

data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type
   deriving Eq

instance Show Error where
   show (Duplicated      n)  = "Duplicated definition: " ++ n
   show (Undefined       n)  = "Undefined: " ++ n
   show (Expected    ty ty') = "Expected: " ++ show ty
                             ++ " Actual: " ++ show ty'

type Env = [(Name, Type)]

-- Implementar
instance Eq VarDef where --para poder usar elem con VarDef
   (VarDef ty1 name1) == (VarDef ty2 name2) = (ty1 == ty2) && (name1 == name2) 

checkProgram :: Program -> [Error]
checkProgram prog = if (checkNames [] prog) == [] then (checkTypes [] prog) else (checkNames [] prog) 

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
      expPsr _ _ = []

checkTypes :: [VarDef] -> Program -> [Error] --llamar solo si checkNames no encuentra errores
checkTypes _ (Program []) = []
checkTypes declaradas (Program ((Decl (VarDef ty name)):cmpStmts)) = checkTypes ((VarDef ty name):declaradas) (Program cmpStmts)
checkTypes declaradas (Program ((Com stmt):cmpStmts)) = (stmtPsr declaradas stmt) ++ (checkTypes declaradas (Program cmpStmts))
   where --NO DECIRLE A LAS SUBEXPRESIONES LO QUE ESPERO SINO QUE DEBEN BUSCAR PROBLEMAS INTERNOS COMO SI ESTUVIESEN AISLADAS Y LUEGO LA GRANDE CHECKEA SUS TIPOS CON UNA FUNCION
      stmtPsr :: [VarDef] -> Stmt -> [Error]
      stmtPsr declaradas stmt
         = case stmt of
               (StmtExpr expr)   -> expPsr declaradas expr
               (If expr bodyIf bodyElse)  
                  -> (expPsr declaradas expr) ++ (genTyErr expr TyInt) ++ (concat (map (stmtPsr declaradas) bodyIf)) ++ (concat (map (stmtPsr declaradas) bodyElse))
               (While expr body) -> (expPsr declaradas expr) ++ (genTyErr expr TyInt) ++ (concat (map (stmtPsr declaradas) body))
               (PutChar expr)    -> (expPsr declaradas expr) ++ (genTyErr expr TyChar)

      expPsr :: [VarDef] -> Expr -> [Error]
      expPsr declaradas (Unary _ expr) = (expPsr declaradas expr) ++ (genTyErr expr TyInt)
      expPsr declaradas (Binary (Equ) expr1 expr2)  = (expPsr declaradas expr1) ++ (expPsr declaradas expr2) ++ (genTyErr expr2 (exprType declaradas expr1))
      expPsr declaradas (Binary (Less) expr1 expr2) = (expPsr declaradas expr1) ++ (expPsr declaradas expr2) ++ (genTyErr expr2 (exprType declaradas expr1))
      expPsr declaradas (Binary _ expr1 expr2)   = (expPsr declaradas expr1) ++ (expPsr declaradas expr2) ++ (genTyErr expr1 TyInt) ++ (genTyErr expr2 TyInt)
      expPsr declaradas (Assign name expr) = (expPsr declaradas expr) ++ (genTyErr expr (exprType declaradas (Var name)))
      expPsr _ _ = []

      exprType :: [VarDef] -> Expr -> Type
      exprType declaradas (Var name)      = if (elem (VarDef TyInt name) declaradas) then TyInt else TyChar
      exprType declaradas (Assign name _) = if (elem (VarDef TyInt name) declaradas) then TyInt else TyChar
      exprType _ (CharLit _)              = TyChar
      exprType _ (GetChar)                = TyChar
      exprType _ _                        = TyInt

      genTyErr :: Expr -> Type -> [Error]
      genTyErr expr ty = if (exprType declaradas expr) == ty then [] else [Expected ty otherty]
         where 
            otherty = case ty of
                        (TyInt)  -> TyChar
                        (TyChar) -> TyInt