module TraverseJVM where

import AbsInstant
import Data.Map as M
import Control.Monad.Reader
import Control.Monad.Error
import Data.List.Split

type Semantics = ReaderT Env IO
type Loc = Int
type Env = M.Map Ident Loc

-- debug helper
f a = g a where g (x :: Int) = x

emptyEnv :: Env
emptyEnv = M.empty

data Ret = Ret {
		stack :: Int,
		code :: String
	} deriving (Show)

emptyRet = Ret { stack = 0, code = ""}

compile :: Program -> String -> IO (String)
compile tree name = do
	-- todo remove extension from name and basedir
	let header = unwords [
		"; author: Michal Wos mw792829570@gmail.com",
		"\n.class public", name,
		"\n.super java/lang/Object",
		"\n.method public <init>()V",
		"\n  aload_0",
		"\n  invokespecial java/lang/Object/<init>()V",
		"\n  return",
		"\n.end method" ]
	content <- runReaderT (transProgram tree) emptyEnv
	let main = unwords [
		"\n.method public static main([Ljava/lang/String;)V",
		--unwords $ Prelude.map (\a -> unwords [" ", a, "\n"]) (splitOn "\n" content),
		content,
		"return",
		"\n.end method\n" ]
	return $ unwords [header, main]


transProgram :: Program -> Semantics (String)
transProgram (Prog stmts) = do
		(env, ret) <- evalStmts stmts
		let stackSize = stack ret
		let localsNum = 1 + M.size env
		let codeJVM = code ret
		return ( unwords [
				"\n .limit stack", show stackSize,
				"\n .limit locals", show localsNum, "\n",
				codeJVM
			] )

evalStmts :: [Stmt] -> Semantics (Env, Ret)
evalStmts (x:xs) = do
	(env, ret1) <- transStmt x
	(env2, ret2) <- local (const env) (evalStmts xs)
	let ret3 = Ret { stack = max (stack ret1) (stack ret2), code = unwords [code ret1, code ret2] }
	return (env2, ret3)
evalStmts [] = do
	env <- ask
	return (env, emptyRet)

transStmt :: Stmt -> Semantics (Env, Ret)
transStmt x = do
	env <- ask
	case x of
		SAss id exp -> do
			ret <- transExp exp
			let newLoc = 1 + M.size env
			let newRet = Ret {
					stack = (stack ret),
					code = unwords [code ret, "istore", show newLoc, "\n"]
				}
			return ( M.insert id newLoc env, newRet )
		SExp exp -> do
			ret <- transExp exp
			let newRet = Ret {
					stack = stack ret,
					code = unwords [code ret, "pop\n"]
				}
			return ( env, newRet )

_transPairExp :: Exp -> Exp -> String -> Semantics Ret
_transPairExp e1 e2 cmd = do
	ret1 <- transExp e1
	ret2 <- transExp e2
	let (a, b) = if (stack ret1 < stack ret2) then (ret2, ret1) else (ret1, ret2)
	return Ret {
			stack = max (stack a) (1 + stack b),
			code = unwords [code a, code b, cmd, "\n"]
		}


transExp :: Exp -> Semantics Ret
transExp x = do
	case x of
		ExpAdd exp1 exp2 -> _transPairExp exp1 exp2 "iadd"
		ExpSub exp1 exp2 -> _transPairExp exp1 exp2 "isub"
		ExpMul exp1 exp2 -> _transPairExp exp1 exp2 "imul"
		ExpDiv exp1 exp2 -> _transPairExp exp1 exp2 "idiv"
		ExpLit n -> return $ Ret { stack = 1, code = unwords ["bipush", (show n), "\n"] }
		ExpVar id -> do
			env <- ask
			let (Just loc) = M.lookup id env
			return $ Ret { stack = 1, code = unwords ["iload", (show loc), "\n"]}
