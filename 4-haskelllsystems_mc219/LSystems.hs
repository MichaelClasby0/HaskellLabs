module LSystems where

import IC.Graphics
import Data.Fixed (mod')

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Angle
angle (angle,_,_) = angle

-- Returns the axiom string for the given system.
axiom :: LSystem -> Axiom
axiom (_,ax,_) = ax

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_,_,axiom) = axiom

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar char rs = concat ([snd r | r <- rs, fst r == char])

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne str rs = concatMap (flip lookupChar rs) str

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand str n rs = head (drop n (iterate (flip expandOne rs) str))

-- Takes an angle in degrees and converts it to radians
radians :: (Floating a) => a -> a
radians x = (x * pi / 180)


-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move 'L' angle (pos, currentAngle) = (pos, (currentAngle + angle) `mod'` 360)
move 'R' angle (pos, currentAngle) = (pos, (currentAngle - angle) `mod'` 360)
move 'F' _ ((x,y), angle) = ((x + (cos . radians) angle, y + (sin . radians) angle) , angle)
move x _ _ = error ( show x ++ " is an invalid movement direction")

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--

--type Vertex = (Float, Float)
--type TurtleState = (Vertex, Float)

initState :: TurtleState
initState = ((0,0), 90)

trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 cmds angle colour = fst (trace1' cmds initState)
  where
    trace1' :: Commands -> TurtleState -> ([ColouredLine],Commands)
    trace1' [] _ = ([],[])
    trace1' (cmd:cmds) state@(currentPos, currentAngle)
      | cmd == 'R' || cmd == 'L' = (nextStep,nextCmds)
      | cmd == ']'               = ([],cmds)
      | cmd == '['               = (bracketTrace ++ trace, commands)
      | otherwise                = ((currentPos, nextPos, colour):nextStep,nextCmds)
        where
          (nextPos,nextAngle) = move cmd angle (currentPos, currentAngle)
          (nextStep,nextCmds) = trace1' cmds (nextPos, nextAngle)
          (bracketTrace, remainingCmds) = trace1' cmds state
          (trace,commands) = trace1' remainingCmds state



trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 cmds angle colour = trace2' cmds initState []
  where
    trace2' :: Commands -> TurtleState -> [TurtleState] -> [ColouredLine]
    trace2' [] _ _ = []
    trace2' (cmd:cmds) state@(currentPos, currentAngle) stack
      | cmd == '['               = trace2' cmds state (state:stack)
      | cmd == ']'               = trace2' cmds pop stackTail
      | cmd == 'R' || cmd == 'L' = nextStep
      | otherwise                = (currentPos, nextPos, colour) : nextStep
        where
          (nextPos,nextAngle) = move cmd angle (currentPos, currentAngle)
          nextStep = trace2' cmds (nextPos, nextAngle) stack
          (pop:stackTail) = stack

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, tree2, arrowHead, peanoGosper, dragon, snowflake, tree, bush, koch, levy :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree2
  = ((45),
      "M",
      [('M', "N[+M]-N"),
       ('N', "NN"),
       ('+', "+"),
       ('-', "-"),
       ('[', "["),
       (']', "]")
      ]
    )
arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (25,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )


koch
  = (90.0,
     "+M",
     [('M', "M-M+M+M-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

levy
  = (45.0,
     "++M",
     [('M', "+M--M+"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
