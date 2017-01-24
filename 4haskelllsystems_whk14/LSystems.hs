module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
-- type System  = (Float, String, Rules) = (Float, String, [(Char, String)])

angle :: System -> Float
angle (a, b, r) = a
--angle (a,_,_) = a

--{angle shape = angle' [shape]
--  where angle' :: [System] -> Float
--	  angle' s = head [a | (a, b, c) <- s]


-- |Returns the base string for the given system.
base :: System -> String
base (a, b, r) = b
--base (_,b,_) = b


-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (a, b, r) = r
--rules (_,_,r) = r


-- |Look up a character in the given set of rules.
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar c r = x 
        where x:xs = [ b | (a, b) <- r, c == a ]
--lookupChar c r = head [ b | (a, b) <- r, c == a ]        

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne r [] = []
expandOne r (c:cs)
 = lookupChar c r ++ expandOne r cs
--use lookupChar to replace


-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand r c n
	| n == 0 = c
	| n == 1 = expandOne r c
	| n >= 2 = expand r (expandOne r c) (n-1)
--	| n > 2          = iterate expandOne r c --??
	| otherwise      = error "Please give n as a non-zero integer."


-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move 'L' ( ( x , y ), deg ) ang = ( ( x , y ), deg+ang )
move 'R' ( ( x , y ), deg ) ang = ( ( x , y ), deg-ang )
move 'F' ( ( x , y ), deg ) ang = ( ( x + cos (deg*rad), y + sin (deg*rad) ), deg)
	where rad = pi / 180


-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.

--type TurtleState   = (Vertex, Float) = position
--type ColouredLine  = (Vertex, Vertex, Colour)

trace :: String -> Float -> Colour -> [ColouredLine]
trace (c:cs) angle colour = trace' (c:cs) angle colour ((0, 0), 90) []

trace' :: String -> Float -> Colour -> TurtleState -> Stack -> [ColouredLine]
trace' [] angle colour position stack = []
trace' (c:cs) angle colour position stack
	| c == 'F'  = (fst position, newPos, colour) : trace' cs angle colour (newPos, newDeg) stack
	| c == '['  = trace' cs angle colour position (position : stack)
	| c == ']'  = trace' cs angle colour (head stack) (drop 1 stack) 
	| otherwise = trace' cs angle colour (newPos, newDeg) stack
		where
		        (newPos, newDeg) = move c position angle
	
-- 	| c == '[' = trace' cs angle colour (new, newang) (position : stack) non exhaustive
--      (position : stack) = put original position into stack
--      head stack = return to previous turtlestate of corresponding '['
--      drop 1 stack = remove head stack from whole [] stack
--      otherwise = 'L' and 'R'

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> Colour -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)
