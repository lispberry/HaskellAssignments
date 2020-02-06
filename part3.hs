import Data.Char (ord, chr, isDigit)

data Expr =
    Num Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Power Expr Expr

wrap op x y = "(" ++ op ++ " " ++ show x ++ " " ++ show y ++ ")"

instance Show Expr where
  show (Num x) = show x
  show (Add x y) = wrap "+" x y
  show (Sub x y) = wrap "-" x y
  show (Mul x y) = wrap "*" x y 
  show (Div x y) = wrap "/" x y
  show (Power x y) = wrap "^" x y

toDigit ch = (ord ch) - (ord '0')
toNum str = foldl (\acc ch -> 10 * acc + (toDigit ch)) 0 str
isNum str = all isDigit str

parse :: String -> Maybe Expr
parse str = 
  case expr $ words str of
    (x, []) -> Just x
    _ -> Nothing
  where
    -- expr = term
    expr s = term s

    -- num = /[0..9]+/
    num (w : s) | isNum w = (Num $ toNum w, s)

    -- power = num '^' power | num
    power s = 
      let (left, s1) = num s in
        case s1 of
        ("^":s2) -> let (right, s3) = power s2 in
          (Power left right, s3)
        _ -> (left, s1)

    -- factor = power {'*'|'/' power}
    factor s =
      let (left, s1) = power s in
        loop left s1 where
          loop left s =
            case s of
            ("*":s1) -> let (right, s2) = power s1 in
              loop (Mul left right) s2
            ("/":s1) -> let (right, s2) = power s1 in
              loop (Div left right) s2 
            _ -> (left, s)

    -- term = factor {'+'|'-' factor}
    term s =
      let (left, s1) = factor s in
        loop left s1 where
          loop left s =
            case s of
            ("+":s1) -> let (right, s2) = factor s1 in
              loop (Add left right) s2
            ("-":s1) -> let (right, s2) = factor s1 in
              loop (Mul left right) s2 
            _ -> (left, s)

polish expr = case expr of
  Num x -> show x
  Add x y -> polish x ++ " " ++ polish y ++ "+"
  Sub x y -> polish x ++ " " ++ polish y ++ "-"
  Mul x y -> polish x ++ " " ++ polish y ++ "*"
  Div x y -> polish x ++ " " ++ polish y ++ "/"
  Power x y -> polish x ++ " " ++ polish y ++ "^"

eval (Num x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Power x y) = eval x ^ eval y