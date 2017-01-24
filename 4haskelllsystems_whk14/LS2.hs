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
angle (a, _, _) = a


-- |Returns the base string for the given system.
base :: System -> String
base (_, b, _) = b


-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, c) = c


-- |Look up a character in the given set of rules.
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar char rules = head [ st | (ch, st) <- rules, ch == char]


-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rules [] = []
expandOne rules (c:chars) = (rule):(expandOne rules chars)
  where rule = lookupChar c rules


-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules [] n = []
expand rules  x 0 = x
expand rules  x n = expand rules (expandOne rules x) (n-1)


-- |Move a turtle.
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move 'F' ((x, y), o) angle = ((x+(cos (o*pi/180)), y+(sin (o*pi/180)) ), o)
move 'L' ((x, y), o) angle = ((x, y), (o+angle) `mod` 360)
move 'R' ((x, y), o) angle = ((x, y), (o-angle) `mod` 360)


-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.

--type TurtleState   = (Vertex, Float) = position
--type ColouredLine  = (Vertex, Vertex, Colour)

trace :: String -> Float -> Colour -> [ColouredLine]
trace commands angle colour
  = trace' commands angle colour ((0, 0), 90) []

trace' :: String -> Float -> Colour -> Position -> Stack -> [ColouredLine]
trace' [] angle colour position stack = stack
trace' (c:commands) angle colour (pos, deg) stack
  | c == 'F'  = (pos, newPos, colour): trace' commands angle colour (newPos, newDeg) stack
  | c == '['  = trace' commands angle colour (pos, deg) ((pos, deg):stack)
  | c == ']'  = trace' commands angle colour (head stack) (drop 1 stack)
  | otherwise = trace' commands angle colour (newPos, newDeg) stack
    where (newPos, newDeg) = move c (pos, deg) angle
--only draw line when moving forward, otherwise just turning

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
