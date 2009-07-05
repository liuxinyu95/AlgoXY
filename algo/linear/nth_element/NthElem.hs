-- file: NthElem.hs
-- Select Nth Element from a list

firstN::Int->[Int]->[Int]
firstN n [] = []
firstN n xs = if i==n then leftPart 
              else
                 if i>n then firstN n leftPart 
                 else leftPart++ firstN (n-i) rightPart
              where 
                leftPart = [x|x<-xs, x<=head xs]
                rightPart = [x|x<-xs, x>head xs]
                i = length leftPart