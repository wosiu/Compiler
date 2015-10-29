module StaticCheck where

import AbsInstant
import Data.Set as S

-- type Semantics = ReaderT Env (StateT St (ErrorT String IO))
type Semantics = ReaderT Env (ErrorT String IO)
type Env = S.Set Ident

--staticCheck :: Program -> String
staticCheck = transProgram

transProgram :: Program -> Semantics Env
transProgram x = case x of
  Prog stmts  -> evalStmts stmts

evalStmts :: [Stmt] -> Semantics Env
evalStmts (x:xs) = do
	env <- transStmt x
	local (const env) (evalStmts xs)
evalStmts [] = do
	env <- ask
	return env

transStmt :: Stmt -> Semantics Env
transStmt x = do
  env <- ask
  case x of
	  SAss id exp  -> do
	    transExp exp
		return $ S.insert id env
	  SExp exp  -> do
	    transExp exp
        return env

_transPairExp :: Exp -> Exp -> Semantics
_transPairExp e1 e2 = do
	transExp e1
	transExp e2

transExp :: Exp -> Semantics
transExp x = case x of
  ExpAdd exp1 exp2  -> _transPairExp exp1 exp2
  ExpSub exp1 exp2  -> _transPairExp exp1 exp2
  ExpMul exp1 exp2  -> _transPairExp exp1 exp2
  ExpDiv exp1 exp2  -> _transPairExp exp1 exp2
  ExpLit n  -> return ()
  ExpVar id  -> do
    env <- ask
    let found = S.member id env
	if found == False then throwError $ (show id) ++ " is undeclared"
	else return ()
