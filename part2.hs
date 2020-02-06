import Data.Bits
import Data.List

nearestCities :: [(String, String, Int)] -> [String]
nearestCities [] = []
nearestCities list = 
  let min = minimum $ map (\(_, _, d) -> d) list in
    nub
    $ (filter (\(_, _, d) -> d == min) list) >>= (\(x, y, _) -> [x, y])

mostWeekends :: [(Int, Int)] -> Int
mostWeekends list = snd $ maximum $ map (\(day, month) -> (count month list, month)) list where
  count x = length . filter (\(_, month) -> month == x)

isSelfDual :: [Int] -> Bool
isSelfDual vec =
  let mask = length vec - 1 in
    all (\(i, f) -> (vec !! i) /= (vec !! mask .&. (complement i)))
    $ zip [0..] vec

game :: Int -> [String] -> String
game _ [x] = x
game k l = loop 1 l [] where
  loop _ [x] [] = x
  loop i [] r = loop i r []
  loop i (e:l) r 
    | i `mod` k == 0 = loop (i + 1) (r ++ l) []
    | otherwise = loop (i + 1) l (r ++ [e])

substr :: String -> String -> Bool
substr "" y = False
substr x y = if starts x y then True else substr (tail x) y where
  starts x y | length x < length y = False
  starts x y = all (\(a, b) -> a == b) $ zip x y