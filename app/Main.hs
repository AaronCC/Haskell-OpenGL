-- | code from: andrew.gibiansky.com/blog/haskell/haskell-gloss/
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data PongGame = Game 
    { ballLoc :: (Float, Float) -- ^ (x, y) location
    , ballVel :: (Float, Float) -- ^ (x, y) velocity
    , player1 :: Float          -- ^ Left player paddle height
                                -- 0 is center of screen
    , player2 :: Float          -- ^ Right player paddle height
    } deriving Show 

width, height, offset, paddleHeight, paddleR, paddleL :: Int
width = 300
height = 300
offset = 100
paddleHeight = 80
paddleR = 120
paddleL = (-120)

fps :: Int
fps = 60

type Radius = Float
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occured
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topCollision    = y - radius <= -fromIntegral width / 2   
        bottomCollision = y + radius >=  fromIntegral width / 2

paddleCollision :: PongGame -> Position -> Radius -> Bool
paddleCollision game (x, y) radius = leftPaddleCol || rightPaddleCol
    where
        leftPaddleCol = (x <= fromIntegral paddleL) 
                        && (y >= (player1 game) && y >= (player1 game) + fromIntegral paddleHeight)
        rightPaddleCol = (x <= fromIntegral paddleL) 
                        && (y >= (player2 game) && y >= (player2 game) + fromIntegral paddleHeight)
                                           
-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx, vy') }
    where
        -- Radius
        radius = 10

        -- Old velocites
        (vx, vy) = ballVel game

        vy' = if paddleCollision game (ballLoc game) radius 
                then
                    -- Update velocity
                    -vy
                    else
                    -- Do nothing
                        vy
                    

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
                mkPaddle rose 120 $ player1 game,
                mkPaddle orange (-120) $ player2 game]
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
            , translate x y $ color paddleColor $ rectangleSolid 20 80
            ]

        paddleColor = light (light blue)

initialState :: PongGame
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (1, -3)
    , player1 = 40
    , player2 = -80
    }

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Update the game by moving the ball
-- Ignore the ViewPort argument
update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . moveBall seconds

main :: IO ()
main = simulate window background fps initialState render update
