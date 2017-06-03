module Tree where


import List
import Int


-------------------
--- BINARY TREE ---
-------------------

data BTree a' = Tip a' | Bin (BTree a', BTree a')
instance Show a' => Show (BTree a') where show t = showBTree "" t 
showBTree str (Tip x)     = " "  ++ show x ++ "\n"
showBTree str (Bin (x,y)) = "+-" ++ showBTree (str ++ "| ") x ++ str ++ 
                            "+-" ++ showBTree (str ++ "  ") y 
tip x   = Tip x
bin x y = Bin(x,y)

foldb h c (Tip x)    = c x
foldb h c (Bin(x,y)) = h (foldb h c x) (foldb h c y)
btree f = foldb bin (tip.f)  
btree' f x y = (btree (\(x,y) -> f x y)) (combine x y)
combine (Tip x)   (Tip y)    = Tip(x,y)
combine (Bin(x,y))(Bin(a,b)) = Bin(combine x a,combine y b)

size  = foldb (+) one 
depth = foldb (curry(succ.(uncurry max))) zero 

--e.g. 
bt = bin
        (bin
            (bin
                (bin
                    (bin(tip 1)(tip 2))
                    (tip 3))
                (bin
                    (bin(tip 4)(tip 5))
                    (bin
                        (tip 6)
                        (bin(tip 7)(tip 8)))))
            (bin(tip 12)(tip 13)))
        (tip 9)



----------------
--   GTREE   ---
----------------

data GTree a' = Node (a', Listl (GTree a'))
instance Show a' => Show (GTree a') where show t = show $ gtree2tree t

-- instance Show a' => Show (GTree a') 
--   where show t = showGTree "   " t
-- showGTree str (Node(a,x)) = "+- " ++ show a ++ "\n" ++ showGForest str x
-- showGForest str Lin = ""
-- showGForest str (Snoc(xs,Node(a,x))) = str ++ "+- " ++ show a ++ "\n" ++
--                                      showGForest (str ++ "|  ") x ++ showGForest str xs
node a x = Node(a,x)

foldg g h d c (Node(a,Lin))        = g a d 
foldg g h d c (Node(a,Snoc(xs,x))) = g a (h (foldg g h d c x)
                                            (((foldl' h c).(listl(foldg g h d c)))xs))
gtree f = foldg (node.f) snoc lin lin
gtree2tree = foldg fork cons nil nil

gsize  = foldg (const succ) (+) 0 0 
gdepth = foldg (const succ) (max) 0 0   

curryg :: GTree a -> BTree a
curryg (Node(a,Lin)) = tip a 
curryg (Node(a,x))   = bin (tip a) (branchg x)
branchg (Snoc(Lin,x))= curryg x
branchg (Snoc(xs,x)) = bin (branchg xs) (curryg x)

uncurryg :: BTree a -> GTree a
uncurryg (Tip a)    = node a lin
uncurryg (Bin(Tip a,Tip b)) = node a (snoc (node b lin) lin) 
uncurryg (Bin(x,Tip b)) = unbranch x (snoc (node b lin) lin)
uncurryg (Bin(x,y)) = unbranch x (snoc (uncurryg y) lin)
unbranch (Tip a) l = node a l
unbranch (Bin(x,Tip a)) l = unbranch x (snoc (node a lin) l)
unbranch (Bin(x,y)) l = unbranch x (snoc (uncurryg y) l)



-- e.g.
-- import Data.Char
-- chrtree      = gtree (chr.fromInteger.(+96)) gt
-- numtree      = gtree ord chrtree
-- chrtree2str  = foldg (++) (++) [] [] $ gtree (\c->[c]) 
gt = Node(1,Snoc(Snoc(Snoc(Snoc(Lin,
         Node(3,Lin)),
         Node(4,Lin)),
         Node(5,Snoc(Snoc(Lin,
             Node(8,Lin)),
             Node(9,Snoc(Snoc(Lin,
                 Node(10,Lin)),
                 Node(2,Lin)))))),
         Node(6,Snoc(Lin,
             Node(7,Lin)))))


------------------
----   TREE   ----
------------------

data Tree a' = Fork (a', Listr (Tree a'))
instance Show a' => Show (Tree a') 
   where show t = showTree "   " t
showTree str (Fork(a,x)) = "+- " ++ show a ++ "\n" ++ showForest str x
showForest str Nil = ""
showForest str (Cons(Fork(a,x),xs)) = str ++ "+- " ++ show a ++ "\n" ++
                                      showForest (str ++ "|  ") x ++ showForest str xs
fork a x = Fork(a,x)

foldt g h d c (Fork(a,Nil)) = g a d
foldt g h d c (Fork(a,xs))  = g a (foldf g h d c xs)
-- foldf g h d c  Nil          = c
foldf g h d c (Cons(x,xs))  = h (foldt g h d c x) (foldf g h d c xs)





-- e.g. 
t = 
    Fork (1,
        Cons(Fork (1,
                Cons(Fork(4,Cons(Fork(6,Nil),Cons(Fork(7,Nil),Nil))),
                Cons(Fork(5,Nil),Nil))),
        Cons(Fork (2,Nil),
        Cons(Fork (3,Nil),Nil))))


