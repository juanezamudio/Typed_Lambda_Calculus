{-# OPTIONS_GHC -Wall #-}

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

import Syntax
import TypeCheck
import Interp

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
      Right x -> if Unsafe `elem` as then
                    case interp x of
                        Left e' -> die (show e')
                        Right e' -> print e'
                 else case checkType x of
                        Left e' -> die (show e')
                        Right _ -> case interp x of
                                    Left e' -> die (show e')
                                    Right e' -> print e'
