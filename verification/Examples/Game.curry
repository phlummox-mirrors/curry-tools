import Nat
import Test.Prop

data Move = L | R

len [] = Z
len (_:xs) = S (len xs)


main = solve  (S (S Z)) (S (S Z))

solve Z Z = []
solve (S x) y = L : solve x y
solve x (S y) = R : solve x y

theorem'solve x y = len (solve x y) -=- add x y
