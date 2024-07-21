module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data BezierState = BezierState { coefs :: [Point]
                               , bezierPoints :: Point
                               , lerpT :: Float}

window :: Display
window = InWindow "bezier curve" (600, 600) (100, 100)

bgColor :: Color
bgColor = makeColor 255 255 255 255

bezierPointSize :: Float
bezierPointSize = 6

pointSize :: Float
pointSize = 6

---

initialState :: BezierState
initialState = BezierState [] (0, 0) 0.0

drawLinesPoints :: [Point] -> [Picture]
drawLinesPoints (x:y:xs) = line [x,y] : drawLinesPoints (y:xs)
drawLinesPoints [_] = []
drawLinesPoints [] = []

drawPoint :: Point -> Float -> Picture
drawPoint (x, y) r = translate x y $ circleSolid r

drawPointBezier :: BezierState -> Picture
drawPointBezier (BezierState ps bp _) = if length ps >= 2 then color red $ drawPoint bp bezierPointSize else Blank

stateAsPicture :: BezierState -> Picture
stateAsPicture bs@(BezierState points _ _) = pictures $ map (`drawPoint` pointSize) points ++
                                                      drawLinesPoints points ++
                                                      [drawPointBezier bs]

handleInputs :: Event -> BezierState -> BezierState
handleInputs (EventKey (MouseButton LeftButton) Up _ mousePos) (BezierState points bp t) =
    BezierState (points ++ [mousePos]) bp t
handleInputs (EventKey (Char 'c') Down _ _) _ = initialState
handleInputs _ points = points

lerp :: Float -> Float -> Float -> Float
lerp s e t = s * (1 - t) + e * t

deCasteljau :: Float -> [(Float, Float)] -> (Float, Float)
deCasteljau _ [b] = b
deCasteljau t coefs_ = deCasteljau t reduced
  where
    reduced = zipWith (lerpP t) coefs_ (tail coefs_)
    lerpP t' (x0, y0) (x1, y1) = (lerp x0 x1 t', lerp y0 y1 t')

bezierStep :: Float -> BezierState -> BezierState
bezierStep t (BezierState ps _ oldT) = BezierState ps newBp newT
    where
        newT = if oldT + t > 1 then 0 else oldT + t
        newBp = deCasteljau newT ps

main :: IO ()
main = play window bgColor 30 initialState stateAsPicture handleInputs bezierStep
