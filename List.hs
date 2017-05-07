module List {--(
Listr(..),Listl(..),
rev, conv,
cons,nil,snoc,lin,
foldr', foldl', listl, listr,
sum', product',
cat, concat',
zero, one, length',
classifier,
filter', filter'',
wrap,nilp,) --} where

import Int

data Listr a = Nil | Cons (a, Listr a)  
data Listl a = Lin | Snoc (Listl a, a)

rev  Lin                  = Nil
rev (Snoc(Lin,a))         = Cons(a, Nil)
rev (Snoc(Snoc(x,b),a))   = Cons(a, rev(Snoc(x,b)))

conv Lin = Nil
conv (Snoc(Lin,a))  = cons a Nil
conv x              = _conv x Nil 
_conv (Snoc(Lin,a)) = cons a 
_conv (Snoc(x,a))   = \y -> _conv x (cons a y)

conv' Nil = Lin
conv' (Cons(a,Nil))  = snoc a Lin
conv' x              = _conv' x Lin 
_conv' (Cons(a,Nil)) = snoc a 
_conv' (Cons(a,x))   = \y -> _conv' x (snoc a y)

showLR Nil = "[]"
showLR (Cons(a, Nil)) = show a
showLR (Cons(a, x))   = show a ++ ", " ++ showLR x
instance (Show a) => Show (Listr a) where show l = "[" ++ showLR l ++ "]"

showLL Lin = "[]"
showLL (Snoc(Lin,a)) = show a
showLL (Snoc(x,a))   = showLL x ++ ", " ++ show a 
instance (Show a) => Show (Listl a) where show l = "[" ++ showLL l ++ "]"

cons a x    = Cons(a,x)
nil         = Nil

snoc a x    = Snoc(x,a)
lin         = Lin

-- foldr' (c,h) [a,..,z] = h a(h b(h c(...(h y(h z c))...)))
foldr' h c Nil          = c 
foldr' h c (Cons (a,x)) = h a (foldr' h c x)  
listr f = foldr' (cons . f) nil

-- foldl' (c,h) = h(h(h(...(h(h c z)y)...)c)b)a
foldl' h c Lin         = c
foldl' h c (Snoc(x,a)) = h a (foldl' h c x)
listl f = foldl' (snoc . f) lin


sum'     = foldr' (+) 0
product' = foldr' (*) 1
cat x y  = conv (foldl' snoc (conv' x) (conv' y))
concat'  = foldr' cat nil
one x    = 1
length'  = sum' . listr one 


classifier p f g = \a -> if p a then f a else g a 

filter' p = concat' . listr (classifier p wrap nilp)
wrap a = cons a nil
nilp a = nil

filter'' p = foldr' (\a l -> if p a then cons a l else l) nil



