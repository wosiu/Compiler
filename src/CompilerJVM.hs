module Main where

import System.IO ( stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs, getProgName )

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
					out <- compileLLVM tree "nazwa" --todo nazwa
					putStr out
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





