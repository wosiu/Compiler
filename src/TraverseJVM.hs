module TraverseJVM where

import AbsInstant
import StaticCheck (staticCheck)
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
	Ident str -> failure x


transProgram :: Program -> Result
transProgram x = case x of
	Prog stmts -> failure x


transStmt :: Stmt -> Result
transStmt x = case x of
	SAss id exp -> failure x
	SExp exp -> failure x


transExp :: Exp -> Result
transExp x = case x of
	ExpAdd exp1 exp2 -> failure x
	ExpSub exp1 exp2 -> failure x
	ExpMul exp1 exp2 -> failure x
	ExpDiv exp1 exp2 -> failure x
	ExpLit n -> failure x
	ExpVar id -> failure x



