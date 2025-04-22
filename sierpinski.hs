data Point = Point Double Double
  deriving (Show)

data Triangle = Triangle Point Point Point
  deriving (Show)

triangle1 = Triangle (Point 0.0 0.0) (Point 1.0 0.0) (Point 0.0 1.0)

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

point1 = Point 0.0 0.0

point2 = Point 0.5 1.5

result = midpoint point1 point2


subTriangles :: Triangle -> [Triangle]
subTriangles (Triangle v1 v2 v3) = 
  let mv1v2 = midpoint v1 v2 
      mv2v3 = midpoint v2 v3
      mv3v1 = midpoint v3 v1
  in [Triangle v1 mv1v2 mv3v1, Triangle v2 mv2v3 mv1v2, Triangle v3 mv3v1 mv2v3]


sub_triangles = subTriangles triangle1 

sierpinski :: Int -> Triangle -> [Triangle]
sierpinski 0 t = [t]
sierpinski n t = concatMap (sierpinski (n-1)) (subTriangles t)


main :: IO ()
main = do
  let n = 5
      triangles = sierpinski n triangle1
  print triangles
