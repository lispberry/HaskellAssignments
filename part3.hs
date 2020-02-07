import Data.Char (ord, chr, isDigit)
import Control.Monad.State

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
  case runState expr (words str) of
    (x, []) -> Just x
    _ -> Nothing
  where
    -- expr = term
    expr = term

    -- num = /[0-9]+/
    num = state $ (\(w:s) -> (Num $ toNum w, s))

    -- power = num '^' power | num
    power = do
      left <- num
      state <- get
      case state of
        ("^":xs) -> do
          put xs
          right <- power
          return $ Power left right
        _ -> do
          return left

    -- factor' = {'*'|'/' power}
    factor' left = do
      state <- get
      case state of
        ("*":xs) -> do
          put xs
          right <- power
          factor' (Mul left right)
        ("/":xs) -> do
          put xs
          right <- power
          factor' (Div left right)
        _ ->
          return left

    -- factor = power factor'
    factor = do
      left <- power
      factor' left
    
    -- term' = {'+'|'-' factor}
    term' left = do
      state <- get
      case state of
        ("+":xs) -> do
          put xs
          right <- factor
          term' (Add left right)
        ("-":xs) -> do
          put xs
          right <- factor
          term' (Sub left right)
        _ ->
          return left

    -- term = factor term'
    term = do
      left <- factor
      term' left

num :: State [String] Expr
num = state $ (\(w:s) -> (Num $ toNum w, s))



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