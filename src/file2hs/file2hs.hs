-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
-- SPDX-License-Identifier: BSD-3-Clause-Clear

module Main where
import System.Environment
import Control.Monad(forM_)

main :: IO ()
main = do
   (modname:varname:_) <- getArgs
   res <- getContents
   putStrLn $ "-- Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.\n-- SPDX-License-Identifier: BSD-3-Clause\n"
   putStrLn $ "module " ++ modname ++ "(" ++ varname ++ ") where"
   putStrLn $ varname ++ " :: String"
   let ls = drop 2 $ lines $ res
   putStrLn $ varname ++ " = concat $"
   putStrLn $ "\t[ " ++ (show $ (head ls)  ++ "\n")
   forM_ (tail ls) $ \ ll -> do
      putStrLn $ "\t, " ++ (show $ ll ++ "\n")
   putStrLn $ "\t]"
