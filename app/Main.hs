module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data PongGame = Game 
    { ballLoc :: (Float, Float) -- ^ (x, y) location
    , ballVel :: (Float, Float) -- ^ (x, y) velocity
    , player1 :: Float          -- ^ Left player paddle height
                                -- 0 is center of screen
    , player2 :: Float          -- ^ Right player paddle height
    } deriving Show 

width, height, offset :: Int
width = 300
height = 300
offset = 100

paddleL, paddleR, paddleH :: Float
paddleH = 80
paddleR = 120
paddleL = (-120)
paddleW = 20

fps :: Int
fps = 60

type Radius = Float
type Position = (Float, Float)

-- | Respond to key events
handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset hte ball to the center
handleKeys (EventKey (Char 's') _ _ _) game =
    game { ballLoc = (0, 0) }
-- Player 2 controls
handleKeys (EventKey (Char 'q') _ _ _) game =
    game { player2 = (player2 game) + 5 }
handleKeys (EventKey (Char 'a') _ _ _) game =
    game { player2 = (player2 game) - 5 }
-- Player 1 controls
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game =
    game { player1 = (player1 game) + 5 }
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game =
    game { player1 = (player1 game) - 5 }
-- Do nothing for all other evets
handleKeys _ game = game

-- | Given position and radius of the ball, return whether a collision occured
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y - radius <= -fromIntegral width / 2   
        bottomCollision = y + radius >=  fromIntegral width / 2

paddleCollision :: PongGame -> Position -> Radius -> Bool
paddleCollision game (x, y) radius = leftPaddleCol || rightPaddleCol
    where
        leftPaddleCol  = (x - radius <= paddleL + (paddleW / 2))
                         && (y <= (player2 game) + (paddleH / 2) 
                            && y >= (player2 game) - (paddleH / 2))
        rightPaddleCol = (x + radius >= paddleR - (paddleW / 2)) 
                         && (y <= (player1 game) + (paddleH / 2)
                            && y >= (player1 game) - (paddleH / 2))
                                           
-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
    where
        -- Radius
        radius = 10

        -- Old velocites
        (vx, vy) = ballVel game

        vx' = if paddleCollision game (ballLoc game) radius 
                then
                    -- Update velocity
                    -vx
                    else
                    -- Do nothing
                        vx
                    

-- Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall
wallBounce :: PongGame -> PongGame
wallBounce game = game  { ballVel = (vx, vy') }
    where
        -- Radius. Use the same thing as in 'render'.
        radius = 10

        -- The old velocites
        (vx, vy) = ballVel game

        vy' = if wallCollision (ballLoc game) radius
                then
                   -- Update the velocity.
                   -vy
                    else
                    -- Do notihng
                        vy

-- | Update the ball position using its current velocity
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state 
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
    where
        -- Old locations and velocites
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        -- New locations
        x' = x + vx * seconds
        y' = y + vy * seconds

render :: PongGame -- ^ Game state
       -> Picture -- ^ Picture to render
render game =
    pictures [ball, walls,
                mkPaddle rose paddleR $ player1 game,
                mkPaddle orange (paddleL) $ player2 game]
    where
        -- The pong ball
        ball = uncurry translate (ballLoc game) 
            $ color ballColor $ circleSolid 10
        ballColor = dark red
        
        -- The bottom and top walls
        wall :: Float -> Picture
        wall offset =
            translate 0 offset $
                color wallColor $
                    rectangleSolid 270 10
                    
        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        -- Make a paddle of a given border and vertical offset
        mkPaddle :: Color -> Float -> Float -> Picture
        mkPaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid 26 86
            , translate x y $ color paddleColor $ rectangleSolid paddleW paddleH 
            ]

        paddleColor = light (light blue)

initialState :: PongGame
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (30, 3)
    , player1 = 40
    , player2 = -80
    }

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Update the game by moving the ball
-- Ignore the ViewPort argument
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds

main :: IO ()
main = print "Welcome to Pong" <* play window background fps initialState render handleKeys update
