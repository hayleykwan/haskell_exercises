module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
-- Given x, search string, & y, list of string/item pairs (a,b)
-- Return [b], list of items whose string matches search string x=a
lookUp x y
  = [ b | (a, b) <- y, x == a  ]


split :: [Char] -> String -> (String, [String])
-- Given 2 strings
-- First string defines separators; Second gives text
-- Return string of punctuations & list of strings (i.e. words)
split s ""  = ("", [""])
split s (x:xs)
  | elem x s  = (x:sep, "":rest)
  | otherwise = (sep, ([x] ++ head rest):tail rest)
  where (sep, rest) = split s xs
        

combine :: String -> [String] -> [String]
combine "" [""]        = [""]
combine "" [x]       = [x]
combine (s:sep) (x:xs) = (x++[s]):(combine sep xs)

--getKeywordDefs :: [String] -> KeywordDefs
-- type KeywordDefs = [(Keyword, KeywordValue)] = [(String, String)] = list of strings
getKeywordDefs []     = []
getKeywordDefs (x:xs) = (w, concat (combine sep words)): getKeywordDefs xs
  where (s:sep, w:words) = split " " x


--expand :: FileContents -> FileContents -> FileContents
-- type FileContents = String
expant text []   = text
expand [] info   = []
expand text info = concat (combine punc (insert words keywordDefs) ) 
  where 
    (punc, words) = split separators text
    (linebreak, defs) = split "\n" info
    keywordDefs = getKeywordDefs defs

-- insert :: [String] -> KeywordDefs -> [String]
--insert text [] = text
insert [] keywords = []
insert (word:words) keywords = (replace word keywords) : (insert words keywords)

-- replace :: String -> KeywordDefs -> String 
replace ('$':i) keywords  = head (lookUp ('$':i) keywords)
replace word keywords = word


main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
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
