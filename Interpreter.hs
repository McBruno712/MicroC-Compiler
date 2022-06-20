-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DEL INTÉRPRETE DEL LENGUAJE DE MÁQUINA

module Interpreter where

import MachineLang

type Conf = (Stack,Env)

type Env = [(Var,Integer)]
type Stack = [Integer]

-- Implementar
interp :: Code -> Code -> Conf -> IO Conf
interp _    []            conf = do --putStrLn ("Finish " ++ (show conf)) 
                                    return conf
interp prev ( SKIP:next)  conf = do --putStrLn ("SKIP " ++ (show conf)) 
                                    interp (SKIP:prev) next conf
interp prev ((PUSH  i):next) (  stk, env) = do  --putStrLn ("PUSH " ++ (show i) ++ " " ++ (show (stk, env))) 
                                                interp ((PUSH  i):prev) next (i:stk, env)
interp prev ((STORE v):next) (i:stk, env) = do  --putStrLn ("STORE " ++ (show v) ++ " " ++ (show (i:stk, env))) 
                                                interp ((STORE v):prev) next (stk, (v, i):(filter ((/= v).fst) env))
interp prev ( MOD:next)    (x:y:stk, env) = do  --putStrLn ("MOD " ++ (show (x:y:stk, env))) 
                                                interp (MOD:prev) next ((x `mod` y):stk, env)
interp prev ( DIV:next)    (x:y:stk, env) = do  --putStrLn ("DIV " ++ (show (x:y:stk, env))) 
                                                interp (DIV:prev) next ((x `div` y):stk, env)
interp prev ( MUL:next)    (x:y:stk, env) = do  --putStrLn ("MUL " ++ (show (x:y:stk, env)))
                                                interp (MUL:prev) next ((x   *   y):stk, env)
interp prev ( SUB:next)    (x:y:stk, env) = do  --putStrLn ("SUB " ++ (show (x:y:stk, env)))
                                                interp (SUB:prev) next ((x   -   y):stk, env)
interp prev ( ADD:next)    (x:y:stk, env) = do  --putStrLn ("ADD " ++ (show (x:y:stk, env)))
                                                interp (ADD:prev) next ((x   +   y):stk, env)
interp prev ( NEG:next)    (x  :stk, env) = do  --putStrLn ("NEG " ++ (show (x:stk, env)))
                                                interp (NEG:prev) next ((       -x):stk, env)
interp prev ( CMP:next)    (x:y:stk, env) = do  --putStrLn ("CMP " ++ (show (x:y:stk, env)))
                                                interp (CMP:prev) next ((x `com` y):stk, env)
   where
      com :: Ord a => a -> a -> Integer
      com v1 v2
         = case (compare v1 v2) of
            GT ->   1
            EQ ->   0
            LT -> (-1)
interp prev ( WRITE:next) (ascii:stk, env) 
   = do  --putStrLn ("WRITE " ++ (show (ascii:stk, env)))
         putChar (toEnum (fromInteger ascii :: Int) :: Char)
         interp (WRITE:prev) next (stk, env)
interp prev ( READ :next) (stk, env)
   = do  --putStrLn ("READ " ++ (show (stk, env)))
         chr <- getChar
         interp (READ:prev) next ((toInteger (fromEnum chr)):stk, env)
interp prev ((LOAD v):next) (stk, env) 
   = do  --putStrLn ("LOAD " ++ v ++ (show (stk, env)))
         interp ((LOAD v):prev) next ((snd (head (filter ((== v).fst) env))):stk, env)
interp prev ((JUMP i):next) conf 
   = do  --putStrLn ("JUMP " ++ (show i) ++ (show conf))
         if (i >= 0) then (interp ((reverse (take (i-1) next)) ++ (JUMP i):prev) (drop (i-1) next) conf)
            else (interp (drop (abs i) prev) ((reverse (take (abs i) prev)) ++ (JUMP i):next) conf)
interp prev ((JMPZ i):next) (t:stk, env)
   = do  --putStrLn ("JMPZ " ++ (show i) ++ (show (t:stk, env)))
         if t == 0 then
            if (i >= 0) then (interp ((reverse (take (i-1) next)) ++ (JMPZ i):prev) (drop (i-1) next) (stk, env))
               else (interp (drop (abs i) prev) ((reverse (take (abs i) prev)) ++ (JMPZ i):next) (stk, env))
            else interp ((JMPZ i):prev) next (stk, env)
