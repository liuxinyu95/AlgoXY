import Data.List

f a b [] = [[]]
f a b (x:xs) = if x == 'A' 
               then [e:es | e<-a, es<-(f (delete e a) b xs)]
               else [e:es | e<-b, es<-(f a (delete e b) xs)]