import Int
import List
import Tree



zipr Nil _ = Nil
zipr _ Nil = Nil
zipr(Cons(a,x))(Cons(b,y)) = Cons((a,b),zipr x y)

unzipr = pair (listr outl, listr outr)
pair (f,g) x = (f x, g x)

unzipr' = foldr' conss nils
nils              = (nil,nil)
conss (a,b) (x,y) = (cons a x,cons b y)


evaln = foldl' f 0
f d n = 10*n + d



