module Main(main) where

import Graphics.Gloss
import Data.List
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Game Of Life" (1000,1000) (0, 0)

background :: Color
background = white

drawing :: Picture
drawing = drawGrid

main :: IO ()
main = do play window background fps aliveCells draw handleKey updateCells 
          where fps = 3
                draw boardIn = pictures [drawGrid, drawAliveCells boardIn]

makeCell :: Bool -> Float -> Float -> Float -> Picture
makeCell isAlive x y size = translate (x - translateX) (y - translateY) $ pictures [ color col $ rectFunc size size ]
                            where col = if isAlive then aliveColour else deadColour
                                  rectFunc = if isAlive then rectangleSolid else rectangleWire
                                  translateX = toFloat (fst gridSize * cellSize) / 2
                                  translateY = toFloat (snd gridSize * cellSize) / 2

aliveColour = red
deadColour = blue

cellSize :: Int
cellSize = 20

gridSize :: (Int, Int)
gridSize = (60, 40)

type Pos = (Int, Int)

data Board = Board {
      isPaused :: Bool,
      cellsAlive :: [Pos],
      size :: (Int, Int)
    } deriving (Show)

aliveCells :: Board
aliveCells = Board True [(4,2), (2,3), (4,3), (3,4), (4,4)] gridSize

-- Need to search a position within a board
inBoard :: Int -> Int -> [Pos] -> Bool
inBoard x y = elem (x,y)

toFloat :: Int -> Float
toFloat a = fromIntegral a :: Float 

-- Draws all the cells in the grid.
drawGrid :: Picture
drawGrid = pictures [ makeCell False (toFloat (x * cellSize)) (toFloat (y * cellSize)) (toFloat cellSize) | 
  x <- [1..fst gridSize], y <- [1..snd gridSize] ]

-- Draw all the alive cells in the grid
drawAliveCells :: Board -> Picture 
drawAliveCells boardIn = pictures [ makeCell True (toFloat (x * cellSize)) (toFloat (y * cellSize)) (toFloat cellSize) | (x,y) <- cellsAlive boardIn ]

-- Update cells
updateCells :: Float -> Board -> Board
updateCells _ (Board True cells size) = Board True cells size 
updateCells _ boardIn = let currentAliveCells = cellsAlive boardIn
                            neighbours = calcNeighboursInBoard currentAliveCells
                            newBoard = neighbours ++ cellsAlive boardIn
                            paused = isPaused boardIn 
                            in Board (isPaused boardIn) (filter (`setAlive` currentAliveCells) newBoard) (size boardIn)

surroundingVectors :: Pos -> [Pos]
surroundingVectors (x, y) = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x - 1, y), (x + 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]

-- Calculate the neighbours of a given position in the board
calcNeighbours :: Pos -> [Pos]
calcNeighbours (x,y) = map (\ (x1, y1) -> (((x1 - 1) `mod` fst gridSize) + 1, ((y1 - 1) `mod` snd gridSize) + 1)) (surroundingVectors (x, y)) 

-- Given the board calculate all of the unique neighbours 
calcNeighboursInBoard :: [Pos] -> [Pos]
calcNeighboursInBoard board = nub (foldr (\pos accum -> calcNeighbours pos ++ accum ) [] board)


-- Calculate whether is alive order
numAliveNeighbours :: Pos -> [Pos] -> Int
numAliveNeighbours pos board = foldr (\pos accum -> if pos `elem` board then accum + 1 else accum) 0 (calcNeighbours pos)

setAlive :: Pos -> [Pos] -> Bool
setAlive pos board | elem pos board && numAlive == 2 = True
                   | numAlive == 3 = True
                   | otherwise = False
  where numAlive = numAliveNeighbours pos board 


toInt :: Float -> Int
toInt = round

calcMouseInGrid :: (Float, Float) -> Board -> Pos
calcMouseInGrid (x, y) boardIn = let offsetX = toFloat $ (fst (size boardIn) * cellSize) `div` 2
                                     offsetY = toFloat $ (snd (size boardIn) * cellSize) `div` 2
                                     relX = toInt ((x + offsetX) / toFloat cellSize)
                                     relY = toInt ((y + offsetY) / toFloat cellSize)
                                  in (relX, relY) 

placeCell :: (Float, Float) -> Board -> Board
placeCell (x, y) boardIn = let (relX, relY) = calcMouseInGrid (x, y) boardIn   
                               boardSize = size boardIn
                               newCells = nub ( (relX, relY) : cellsAlive boardIn )
                            in if relX < fst boardSize && relX > 0 && relY < fst boardSize && relY > 0 then boardIn { cellsAlive = newCells } else boardIn

deleteCell :: (Float, Float) -> Board -> Board
deleteCell (x, y) boardIn = let (relX, relY) = calcMouseInGrid (x, y) boardIn
                            in boardIn { cellsAlive = delete (relX, relY) (cellsAlive boardIn) } 

-- Handle the pausing events and the user pressing a square to add to the board.
handleKey :: Event -> Board -> Board
handleKey (EventKey (Char 'p') Up _ _) game = game { isPaused = not $ isPaused game}
handleKey (EventKey (MouseButton LeftButton) Up _ pos) game = placeCell pos game 
handleKey (EventKey (MouseButton RightButton) Up _ pos) game = deleteCell pos game

handleKey _ game = game












