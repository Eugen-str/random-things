module Main where

import Graphics.Gloss

win :: Display
win = InWindow "Sierpinski" (800, 800) (200, 200)

bgColor :: Color
bgColor = makeColorI 255 255 255 255

sierpinski :: Int -> Path -> [Picture]
sierpinski 0 _ = [Blank]
sierpinski n [a@(ax,ay), b@(bx,by), c@(cx,cy)] = image : (left ++ right ++ top)
    where
        h_ac  = ((cx+ax)/2, (cy+ay)/2)
        h_ab  = (cx, by)
        h_bc  = ((cx+bx)/2, (cy+by)/2)
        image = lineLoop [h_ac, h_ab, h_bc]
        top   = sierpinski (n - 1) [h_ac, h_bc, c]
        right = sierpinski (n - 1) [h_ab, b, h_bc]
        left  = sierpinski (n - 1) [a, h_ab, h_ac]
sierpinski _ _ = error "wtf"


pic :: Picture
pic = pictures $ sierpinski 10 initTri
               ++ [lineLoop initTri]
    where
        initTri = [(-200,-200), (200, -200), (0, 146.41)]

main :: IO ()
main = display win bgColor pic
