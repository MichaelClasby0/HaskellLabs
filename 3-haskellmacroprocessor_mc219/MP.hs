module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp str pairs = [ value | (key,value) <- pairs, key == str]


splitText :: [Char] -> String -> (String,[String])
splitText seps str = splitText' str "" "" []
  where
    splitText' :: String -> String -> String -> [String] -> (String,[String])
    splitText' []      word seps' out = (reverse seps', reverse (map reverse (word:out)))
    splitText' (s:str) word seps' out
      | s `elem` seps = splitText' str "" (s:seps') (word:out)
      | otherwise     = splitText' str (s:word) seps' out


combine :: String -> [String] -> [String]
combine (s:seps) (w:words') = w : [s] : combine seps words'
combine _ w                 = w


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs lines' = [splitAtSpace x "" | x <- lines']
  where
    splitAtSpace :: String -> String -> (Keyword, KeywordValue)
    splitAtSpace [] front = (reverse front ,"") -- When input has no spaces
    splitAtSpace (s:str) front
      | s == ' '  = (reverse front ,str)
      | otherwise = splitAtSpace str (s:front)


expands :: FileContents -> FileContents -> FileContents
expands text info
  = expand' (splitText separators text) (getKeywordDefs $ snd (splitText "\n" info))
  where
    expand' :: (String,[String]) -> KeywordDefs -> FileContents
    expand' (s,w) defs = concat (uncurry combine split')
      where
        split' = (s,replace w)
        replace :: [String] -> [String]
        replace [] = []
        replace (w':words')
          | w' == ""       = w' : replace words'
          | head w' == '$' = (concat $ lookUp w' defs) : replace words'
          | otherwise      = w' : replace words'


--replaceWord :: String -> KeywordDefs -> String
--replaceWord word defs =  concat $ lookUp word defs

-- split text file by seperators
-- use info file to define KeywordDefs
-- swap keyword with KeywordValue using lookup
-- combine list back together

-----------------------------------------------------

-- EXTENSION
expand :: FileContents -> FileContents -> FileContents
expand text infos
  | '#' `elem` infos = concat (reassemble (map (expands text) (filter (/= "") (snd (splitText "#" infos)))))
  | otherwise        = expands text infos
  where
    reassemble :: [String] -> [String]
    reassemble []      = []
    reassemble (s:str) = s : "----- \n" : reassemble str

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
