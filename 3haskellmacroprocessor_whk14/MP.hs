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
	= [ b | (a,b) <- y, a == x ]


split :: [Char] -> String -> (String, [String])
-- Given 2 strings
-- First string defines separators; Second gives text
-- Return string of punctuations & list of strings (i.e. words)
split sep "" = ("", [""])
split sep (x:xs)
	| elem x sep = ( x : sep1, "" : sep2 )
	| otherwise  = ( sep1 , ([x] ++ head) : tail )
		where (sep1, sep2) = (split sep xs)
		      (head : tail) = sep2
--pattern matching: sep1 is fst(split sep xs), type: String
--pattern matching: sep2 is snd(split sep xs), type: list of String
--pattern matching: head, type: String
--pattern matching: tail, type: list of String
-- "++" (concatenate) joins two lists of same type to form new list


combine :: String -> [String] -> [String]
combine "" [""] = [""]
combine "" [x]  = [x]
combine (sep:seps) (x:xs)
	= ( x : [sep] : combine seps xs )
-- x is text which should always come first
-- x is string, sep is string, [sep] is list of string, combine is [string]


getKeywordDefs :: [String] -> KeywordDefs
-- type KeywordDefs = [(Keyword, KeywordValue)] = [(String, String)] = list of strings
-- split :: [Char] -> String -> (String, [String])
-- combine :: String -> [String] -> [String]
getKeywordDefs [] = []
getKeywordDefs (a:as)
 = (y, concat (combine xs ys) ) : getKeywordDefs as
	where ((x:xs), (y:ys)) = split " " a
-- x:xs is separators (String), y:ys words separated ([String])
-- y is keyword, ys is concat


expand :: FileContents -> FileContents -> FileContents
-- type FileContents = String
expand template []  = error "no input"
expand [] keywords  = error "no template"
expand template keywords
 = concat ( combine sep (expand' temp keywordV) )
	where   (sep, temp) = split separators template  
			--returns ("sep", ["The","capital","of","$1","is",$2"])
		(and, keyw)  = split "\n" keywords  
			--returns ("\n", ["$1 Peru", "$2 Lima"])
		keywordV     = getKeywordDefs keyw  
			--returns [("$1","Peru"), ("$2","Lima")]

expand' :: [String] -> KeywordDefs -> [String]
expand' [] keywordV = [] --base case
expand' (temp:temps) keywordV = (replaceWord temp keywordV) : expand' temps keywordV
-- replace $a to $z til no more temp

replaceWord :: String -> KeywordDefs -> String
replaceWord ('$': and) keywordV = head ( lookUp ('$': and) keywordV )
			-- keywordV input: ["$1 Peru", "$2 Lima"]
			-- return Peru, Lima
			-- ( lookUp ('$': and) keywordV ) returns [Peru]
			-- head removes the list
replaceWord w keywordV          = w

--1. split input into sep, words
--2. getKeywordDefs
--3. replace keywords '$' into keywordV
--4. combine string


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

