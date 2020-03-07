module Main where

import System.Random

main :: IO ()
main = do
  putStrLn "Passwords"
  putStrLn "---"
  gen <- getStdGen
  passwords <- generatePasswords 10 gen
  mapM_ putStrLn . map copyToClipboard $ passwords

generatePasswords :: (RandomGen g) => Int -> g -> IO [String]
generatePasswords 0 _ = return []
generatePasswords n gen = do
  let password = generatePassword gen
  gen <- newStdGen
  remainingPasswords <- generatePasswords (n - 1) gen
  return $ password : remainingPasswords

generatePassword :: (RandomGen g) => g -> String
generatePassword = take 16 . filterChars . randomRs ('0','z')

filterChars :: String -> String
filterChars =
  let allowedChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
   in filter (`elem` allowedChars)

copyToClipboard :: String -> String
copyToClipboard str =
   str ++ " | bash='/bin/bash' param1='-c' param2='/bin/echo -n " ++ str ++ "\\c | pbcopy' terminal=false"