module List where
import Int

data Listr a = Nil | Cons (a, Listr a)  
data Listl a = Lin | Snoc (Listl a, a)

instance (Show a)=>Show(Listr a) where show l = "[" ++ showLr l ++ "]"
showLr Nil = "[]"
showLr (Cons(a, Nil)) = show a
showLr (Cons(a, x))   = show a ++ ", " ++ showLr x
instance (Show a)=>Show(Listl a) where show l = "[" ++ showLl l ++ "]"
showLl Lin = "[]"
showLl (Snoc(Lin,a)) = show a
showLl (Snoc(x,a))   = showLl x ++ ", " ++ show a 

nil = Nil;   cons a x = Cons(a,x)
lin = Lin;   snoc a x = Snoc(x,a)

ll = snoc 1 (snoc 2 (snoc 3 (snoc 4 (snoc 5 (snoc 7 lin)))))
lr = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 7 nil)))))

revl = foldl' cons nil 
revr = foldr' snoc lin

convl  Lin          = Nil
convl (Snoc(x,a))   = snocr (convl x) a 
snocr  Nil        a = cons a nil
snocr (Cons(a,x)) b = cons a (snocr x b)

convr  Nil          = Lin
convr (Cons(a,x))   = consl a (convr x)
consl a  Lin        = snoc a lin
consl a (Snoc(x,b)) = snoc b (consl a x)

-- foldr' h c [1,2,3,4] = h 1(h 2(h 3(h 4 c)))
-- foldl' h c [1,2,3,4] = h(h(h(h c 4)3)2)1
foldr' h c Nil          = c 
foldr' h c (Cons (a,x)) = h a (foldr' h c x)  
listr f = foldr' (cons . f) nil

foldl' h c Lin         = c
foldl' h c (Snoc(x,a)) = h a (foldl' h c x)
listl f = foldl' (snoc . f) lin

sumr     = foldr' (+) 0
suml     = foldl' (+) 0
productr = foldr' (*) 1
productl = foldl' (*) 1

cat x y  = 
cat x y  = convl (foldl' snoc (convr x) (convr y))
concat'  = foldr' cat nil
one x    = 1
length'  = sum' . listr one 


classifier p f g a = if p a then f a else g a 

filter' p = concat' . listr (classifier p wrap nilp)
wrap a = cons a nil
nilp a = nil

filter'' p = foldr' (\a l -> if p a then cons a l else l) nil



