import Data.Char (ord, chr, isAlpha, isUpper, isLower)
import System.IO

shift :: Int -> Char -> Char
shift n c
  | isUpper c = int2let $ (let2int c + n) `mod` 26
  | isLower c = int2let $ (let2int c + n) `mod` 26
  | otherwise = c

let2int :: Char -> Int
let2int c
  | isUpper c = ord c - ord 'A'
  | isLower c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n [] = []
decode n ('&' : 'N' : 'B' : 'S' : 'P' : ';' : rest) = ' ' : decode n rest
decode n (x : xs) = if isAlpha x then shift (-n) x : decode n xs else x : decode n xs

encryptFile :: String -> String -> IO ()
encryptFile inputPath outputFileName = do
  putStrLn "Enter encryption key (an integer):"
  keyStr <- getLine
  let key = read keyStr :: Int
  contents <- readFile inputPath
  let encrypted = encode key contents
  writeFile outputFileName encrypted
  putStrLn "Encryption complete."

decryptFile :: String -> String -> IO ()
decryptFile inputPath outputFileName = do
  putStrLn "Enter decryption key (an integer):"
  keyStr <- getLine
  let key = read keyStr :: Int
  contents <- readFile inputPath
  let decrypted = decode key contents
  writeFile outputFileName decrypted
  putStrLn "Decryption complete."

main :: IO ()
main = do
  putStrLn "Welcome to CYPHER!"
  loop

loop :: IO ()
loop = do
  putStrLn "Choose an option:"
  putStrLn "1. Encrypt"
  putStrLn "2. Decrypt"
  putStrLn "3. Quit"
  optionStr <- getLine
  case optionStr of
    "1" -> do
      putStrLn "Enter the input file name:"
      inputFileName <- getLine
      putStrLn "Enter the output file name:"
      outputFileName <- getLine
      encryptFile inputFileName outputFileName
      loop
    "2" -> do
      putStrLn "Enter the input file name:"
      inputFileName <- getLine
      putStrLn "Enter the output file name:"
      outputFileName <- getLine
      decryptFile inputFileName outputFileName
      loop
    "3" -> putStrLn "Exiting."
    _   -> do
      putStrLn "Invalid option. Please try again."
      loop
