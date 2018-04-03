data Poly a =
    X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving Show
-- data Statement a = Coef a | Function (Statement a) | Sum (Statement a) (Statement a) | Prod (Statement a) (Statement a) 
posIntsUpTo :: Int -> [Int] --used across list comprehensions for prettier notation
posIntsUpTo n = take n [0,1..]

clean0sOut  :: [Integer] -> [Integer] --used for polyListDegree so that in the case of a polynomial
clean0sOut  x = reverse $ dropWhile(==0) $ reverse x

getPolyList    ::  FilePath -> IO [Integer]
getPolyList path =  do {xs <- readFile path; return $ map read $ lines xs} --I just like doing everything at once, also dolla bill operator is my favorite thing

polyListValue :: [Integer] -> Integer -> Integer 
polyListValue [] _ = 0
polyListValue  p n = sum [ (p !! powerOfTerm) * (n^powerOfTerm) | powerOfTerm <- posIntsUpTo $ length p  ] 
--very simply put, the value of p(x) @ x is the sum of the products of the coefficients and their respective power of x

polyListDegree :: [Integer] -> Integer
polyListDegree p 
| length(p') < 1 = undefined --as specified for requirement
| otherwise = toInteger $ length p' - 1 --p' is there to act just incase you add a malicious tail of zeroes to artificially inflate the length, or as a result of some computation
            where
                p' = clean0sOut p 

polyListSum :: [Integer] -> [Integer] -> [Integer]
polyListSum p q = if length p >= length q then [ (p !! powerOfTerm) + (q' !! powerOfTerm) | powerOfTerm <- posIntsUpTo $ length p] --the set of polynomialLists is closed under addition
                                      else [ (p' !! powerOfTerm) + (q !! powerOfTerm) | powerOfTerm <- posIntsUpTo $ length q] --case of len q > len p
              where
                diffPQ = abs $ length q - length p --incase of different lengths
                list0s  = replicate diffPQ 0       --one must account for more zeroes 
                p' = p ++ list0s
                q' = q ++ list0s

polyListProd :: [Integer] -> [Integer] -> [Integer] --theres a very nice little sigma solution to this question:
polyListProd [] _ = [] --given an empty/Zero Polynomial multiplied any polynomial it will return another Zero Polynomial
polyListProd _ [] = [] 
polyListProd p q  = [ sum [ (p' !! index) * (q' !! abs(powerOfTerm-index)) | index <- posIntsUpTo (powerOfTerm+1) ] | powerOfTerm <- posIntsUpTo $ combinedDeginInt+1 ]--given two polynomials p and q with existant degrees
        where --their product is represented by (sigma for var power:(sum of degrees) start@power=0 (sigma for var index:(power) start@index=0 (p_index*q_(power-index))))
              pDeg = polyListDegree p --the reason why in the comprehension you need to +1 each index limit is because 
              qDeg = polyListDegree q --sigma runs from [start..index limit] where posIntsUpTo returns [0..index limit-1]
              combinedDeginInt = fromIntegral $ (pDeg+qDeg)
              smallerDiff= abs $ combinedDeginInt - (abs $ length q - length p)
              biggerDiff = combinedDeginInt
              smaller0s  = replicate smallerDiff 0 --in order to make sure that there is no index error,
              bigger0s   = replicate biggerDiff  0 --you fill the smaller/bigger polynomial with tailing zeroes
              p' = if pDeg>=qDeg then p ++ smaller0s else p ++ bigger0s 
              q' = if qDeg>=pDeg then q ++ smaller0s else q ++ bigger0s 

polyListDeriv :: [Integer] -> [Integer]
polyListDeriv [] = [] --derivative of zero Polynomial is always Zero Polynomial
polyListDeriv p  = tail [ (p !! i) * (toInteger i) | i <- posIntsUpTo $ length p ] --otherwise the derivative of any polynomial will be the tail of that list with every element multiplied by its previous index

polyListToPoly :: [Integer] -> Poly
polyListToPoly []     = (Coef 0) --pretty self explanatory
polyListToPoly (c:cs) = Sum (Coef c) (Prod X (polyListToPoly cs)) --Horners method of reppin polys

polyToPolyList ::  Poly -> [Integer] -- we can break this down like we'd break down any fn that takes a Poly
polyToPolyList (Coef n)    = [n] --case of coefficient
polyToPolyList X           = [0,1] --case of X
polyToPolyList (Sum  p q)  = polyListSum   (polyToPolyList p)  (polyToPolyList q) --case of sum
polyToPolyList (Prod p q)  = polyListProd  (polyToPolyList p)  (polyToPolyList q) --case of product
