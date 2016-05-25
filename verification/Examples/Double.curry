import Nat
import Test.Prop

double x = add x x

coin x = x ? S x

even Z = True
even (S Z) = False
even (S (S n)) = even n

theorem'evendoublecoin x = always (even (double (coin x)))

