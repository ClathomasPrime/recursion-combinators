
data Fix f = Fix { unFix :: f (Fix f) }


data NatF a = SucF a | ZeroF
type Nat = Fix NatF

suc :: Nat -> Nat
suc x = Fix . SucF $ x

zero :: Nat
zero = Fix ZeroF


data ListF a b = ConsF a b | NilF
type List a = Fix (ListF a)

cons :: a -> List a -> List a
cons a list = Fix . ConsF a $ list

nil :: List a 
nil = Fix NilF


cata :: 
