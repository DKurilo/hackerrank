import Text.Printf
import qualified Data.List as DL
import Debug.Trace

type Point = (Double,Double)
type Vector = (Double,Double)

solve :: [Point] -> Double
solve points = perimeter . convexhull . DL.sort $ points

perimeter :: [Point] -> Double
perimeter [] = 0
perimeter (_:[]) = 0
perimeter (p:ps) = per + distance lp p
    where (lp, per) = 
              foldl (\(p'',per'') p' -> (p',per'' + distance p' p'')) (p, 0) ps

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

convexhull :: [Point] -> [Point]
convexhull [] = []
convexhull (p:ps) = convexhull' p (p:ps) (1,0)

convexhull' :: Point -> [Point] -> Vector -> [Point]
convexhull' fp ((x,y):ps) v
    | np == fp = [fp]
    | otherwise = (np:(convexhull' fp (np:filter (/=np) ps) $ vec np))
    where vec p = (fst p - x, snd p - y)
          np = snd . maximum $ map (\p' -> let c = vcos v (vec p') in
                   (if (x,y)==fp && snd p' < y then 2-c else c, p')) $ 
               filter (/=(x,y)) $ (fp:ps)

vcos :: Vector -> Vector -> Double
vcos (x1,y1) (x2,y2) = (x1*x2+y1*y2)/(sqrt ((x1*x1+y1*y1)*(x2*x2+y2*y2)))

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Double)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans

