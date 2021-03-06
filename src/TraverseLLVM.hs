module TraverseLLVM where

import AbsInstant
import Data.Set as S
import Control.Monad.Reader
import Control.Monad.Error
import Data.List.Split

type Semantics = ReaderT Env IO

type VEnv = S.Set Ident

data Env = Env {
		nextReg :: Int,
		declaredVars :: VEnv
	}


-- debug helper
f a = g a where g (x :: Int) = x

emptyEnv :: Env
emptyEnv = Env {
           		nextReg = 1,
           		declaredVars = S.empty
           	}

data Ret = Ret {
		value :: String,
		code :: String
	} deriving (Show)

emptyRet = Ret { value = "", code = ""}

compile :: Program -> String -> IO (String)
compile tree name = do
	let header = unwords [
			"; author Michal Wos mw792829570@gmail.com",
			"\n@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1",
			"\ndeclare i32 @printf(i8*, ...) #1"
		]
	content <- runReaderT (transProgram tree) emptyEnv
	let main = unwords [
		"\ndefine i32 @main() #0 {\n",
		content,
		"ret i32 0",
		"\n}"]
	return $ unwords [header, main]


transProgram :: Program -> Semantics (String)
transProgram (Prog stmts) = do
		(ret, _) <- evalStmts stmts
		return $ code ret

evalStmts :: [Stmt] -> Semantics (Ret, Env)
evalStmts (x:xs) = do
	(ret1, env) <- transStmt x
	(ret2, env2) <- local (const env) (evalStmts xs)
	let ret3 = Ret { value = "", code = unwords [code ret1, code ret2] }
	return (ret3, env2)
evalStmts [] = do
	env <- ask
	return (emptyRet, env)

transId :: Ident -> String
transId (Ident ident) = "%" ++ ident

transStmt :: Stmt -> Semantics (Ret, Env)
transStmt x = do
	case x of
		SAss id exp -> do
			(ret, env) <- transExp exp
			let varReg = transId id
			let resVal = value ret
			if S.member id (declaredVars env) then do
				let newRet = Ret {
						value = "", -- unused
						code = unwords [
							code ret,
							("store i32 " ++ resVal ++ ", i32* " ++ varReg ++ ", align 4"), "\n"]
					}
				return (newRet, env)
			else do
				let newRet = Ret {
						value = "", -- unused
						code = unwords [
							varReg, "= alloca i32, align 4\n",
							code ret,
							("store i32 " ++ resVal ++ ", i32* " ++ varReg ++ ", align 4"), "\n"]
					}
				let newEnv = Env {
					nextReg = (nextReg env),
					declaredVars = (S.insert id (declaredVars env))
				}
				return (newRet, newEnv)
		SExp exp -> do
			(ret, env) <- transExp exp
			let newReg = "%" ++ (show (nextReg env))
			let newRet = Ret {
               		value = "", -- unused
               		code = unwords [
               			code ret,
               			newReg,
               			" = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0,i32 0), i32 "
               			++ (value ret) ++ ")\n" ]
               	}
			return (newRet, (increaseReg env))

_transPairExp :: Exp -> Exp -> String -> Semantics (Ret, Env)
_transPairExp e1 e2 cmd = do
	(ret1, env1) <- transExp e1
	(ret2, env2) <- local (const env1) (transExp e2)
	let val = "%" ++ (show (nextReg env2))
	let cd = val ++ " = " ++ cmd ++ " i32 " ++ (value ret1) ++ ", " ++ (value ret2) ++ "\n"
	return ( Ret {
			value = val,
			code = unwords [code ret1, code ret2, cd]
		}, (increaseReg env2) )


transExp :: Exp -> Semantics (Ret, Env)
transExp x = do
	case x of
		ExpAdd exp1 exp2 -> _transPairExp exp1 exp2 "add nsw"
		ExpSub exp1 exp2 -> _transPairExp exp1 exp2 "sub nsw"
		ExpMul exp1 exp2 -> _transPairExp exp1 exp2 "mul nsw"
		ExpDiv exp1 exp2 -> _transPairExp exp1 exp2 "sdiv"
		ExpLit n -> do
			reg <- ask
			return ( Ret { value = (show n), code = "" }, reg )
		ExpVar id -> do
			env <- ask
			let val = "%" ++ (show (nextReg env))
			let cd = val ++ " = load i32* " ++ (transId id) ++ ", align 4\n"
			return ( Ret { value = val, code = cd }, (increaseReg env) )


increaseReg :: Env -> Env
increaseReg env =
	Env {
		nextReg = (nextReg env) + 1,
		declaredVars = declaredVars env
	}