module Main where

import System.IO ( stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs, getProgName )
import Data.List
import System.Process

import ErrM
import LexInstant
import ParInstant
import PrintInstant
import AbsInstant
import StaticCheck ( staticCheck )
import TraverseJVM ( compileJVM )
import TraverseLLVM ( compileLLVM )

myLLexer = myLexer

putErr :: String -> IO ()
putErr s = hPutStrLn stderr s

runFile :: FilePath -> IO ()
runFile path = readFile path >>= run path

runLLVM :: Program -> String -> String -> IO ()
runLLVM tree basedir name = do
	out <- compileLLVM tree name
	let newPath = (basedir ++ name ++ ".ll")
	writeFile newPath out
	_ <- runCommand $ "llvm-as " ++ newPath
	return ()

runJVM :: Program -> String -> String -> IO ()
runJVM tree basedir name = do
	out <- compileJVM tree name
	let newPath = (basedir ++ name ++ ".j")
	writeFile newPath out
	_ <- runCommand $ unwords ["java -jar lib/jasmin.jar -d", basedir, newPath, ">> /dev/null"]
	return ()

run :: String -> String -> IO ()
run path code = do
	let ts = myLLexer code
	case pProgram ts of
		Bad s -> do
			putErr $ "Parsing failed: " ++ show s
			putErr "Tokens:"
			putErr $ show ts
		Ok tree -> do
			isCorrect <- staticCheck tree
			case isCorrect of
				True -> do
					let (Just i) = findIndex (\a -> a == '/') (reverse path)
					let (basedir, file) = splitAt (length path - i) path
					let (Just j) = findIndex (\a -> a == '.') (reverse file)
					let name = take (length file - j - 1) file
					runJVM tree basedir name
					return ()
				False -> return ()


showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
	putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
	putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
	args <- getArgs
	case args of
		--todo
		--[] -> hGetContents stdin >>= run pProgram
		path -> mapM_ runFile path





