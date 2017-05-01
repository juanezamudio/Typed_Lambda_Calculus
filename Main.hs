{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (succ, pred, fst, snd)
--import Data.Map (Map)
--import qualified Data.Map as Map

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

import Syntax
import TypeCheck
import Interp
import Test
--type Store = Map VarName Exp

--update :: VarName -> Exp -> Store -> Store
--update v n st = Map.insert v n st


data Flag = Unsafe               -- -u
          | Help                 -- --help
           deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option ['u'] []       (NoArg Unsafe)
        "Disable Type-checking"
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ]

parseArgs :: [String] -> IO ([Flag], [[Char]])
parseArgs argv = case getOpt Permute flags argv of
      (args,fs,[]) -> do
          let files = if null fs then ["-"] else fs
          if Help `elem` args
              then do hPutStrLn stderr (usageInfo header flags)
                      exitWith ExitSuccess
              else return (args, files)

      (_,_,errs)      -> do
          hPutStrLn stderr (concat errs ++ usageInfo header flags)
          exitWith (ExitFailure 1)

      where header = "Usage: interp [-u] [file]"

main :: IO ()
main = do
    (as, fs) <- getArgs >>= parseArgs
    content <- if head fs == "-" then getContents else readFile (head fs)
    case tryParse lcSyntax content of
      Left e -> die e
      Right x -> if Unsafe `elem` as then print $ interp x
                 else case check x of
                        Left e -> die (show e)
                        Right _ -> print $ interp x
