module Main where

import Graphics.Gloss

win :: Display
win = InWindow "Sierpinski" (800, 800) (200, 200)

bgColor :: Color
bgColor = makeColorI 255 255 255 255

sierpinski :: Int -> Path -> [Picture]
sierpinski 0 _ = [Blank]
sierpinski n xs@[a@(ax,ay), b@(bx,by), c@(cx,cy)] = image : (left ++ right ++ top)
    where
        image = lineLoop xs
        top   = sierpinski (n - 1) [((ax+cx)/2,(ay+cy)/2), ((bx+cx)/2,(by+cy)/2), c]
        right = sierpinski (n - 1) [(cx,by), b, ((cx+bx)/2,(cy+by)/2)]
        left  = sierpinski (n - 1) [a, (cx,ay), ((cx+ax)/2,(cy+ay)/2)]
sierpinski _ _ = error "wtf"


pic :: Picture
pic = pictures $ sierpinski 3 [(-200,-200), (200, -200), (0, 146.41)]

main :: IO ()
main = display win bgColor pic
