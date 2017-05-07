module Int where 

foldn (c,h) 0 = c
foldn (c,h) n = h (foldn (c,h) (n-1))

plus m = foldn (m, succ)
mult m = foldn (0, plus m)
expn m = foldn (1, mult m)

outl(m,n) = m
outr(m,n) = n

f (m,n) = (m+1, n*(m+1))
fact = outr . (foldn ((0,1), f))

g (m,n) = (n, m+n)
fib = outl . foldn ((0,1), g)

h s = (foldn (1,s)) . (+1) 
ack = foldn ((+1), h)

zero x = 0

