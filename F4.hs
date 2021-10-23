{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module F4 where

data F4 = Zero | One | U | V
  deriving (Eq)

instance Show F4 where
  show Zero = "0"
  show One  = "1"
  show U    = "u"
  show V    = "v"

instance Num F4 where
  -- Addition
  Zero + a = a
  One  + One  = Zero
  One  + U    = V
  One  + V    = U
  U    + U    = Zero
  V    + V    = Zero
  U    + V    = One
  a    + b    = b + a
  negate x = x
  -- Multiplikation
  Zero * _ = Zero
  One  * a = a
  U    * U = V
  U    * V = One
  V    * V = V + One
  a    * b = b * a
  abs x = x
  signum x = 1
  fromInteger x
    | even x = Zero
    | otherwise = One

class (Monoid v, Num k) => Vectorspace k v | v -> k where
  -- Skalar multiplikation
  (^*) :: k -> v -> v
  (*^) :: v -> k -> v
  a *^ b = b ^* a
  -- Vektoraddition
  (^+) :: v -> v -> v
  (^+) = mappend

-- | Vectorspace of dimension 3 over the field F4
data F43 = F43 F4 F4 F4
  deriving (Eq)

instance Show F43 where
  show (F43 a b c) = "("
                    <> show a
                    <> " , "
                    <> show b
                    <> " , "
                    <> show c
                    <> ")"

instance Semigroup F43 where
  F43 a b c <> F43 a' b' c' = F43 (a+a') (b+b') (c+c')

instance Monoid F43 where
  mempty = F43 Zero Zero Zero

instance Vectorspace F4 F43 where
  x ^* F43 a b c = F43 (x*a) (x*b) (x*c)

-- | Projective space over a vectorspace a 
newtype P a = P a

-- | Declare the PF_4^2 to be P(F_4^3)
type PF42 = P F43

instance Show PF42 where
  show (P (F43 a b c)) = "("
                    <> show a
                    <> " : "
                    <> show b
                    <> " : "
                    <> show c
                    <> ")"

instance Eq PF42 where
  P v == P w =       v == w
             || U ^* v == w
             || V ^* v == w
             || V ^* v == w
             ||      v == w *^ U
             ||      v == w *^ V
             || U ^* v == w *^ U
             || V ^* v == w *^ U
             || U ^* v == w *^ V
             || U ^* v == w *^ V

-- | all elements of the finite field F4
elementsOfF4 = [Zero, One, U, V]

-- | List all distinct elements of PF42
-- elementsOfPF42 :: Set.Set PF42
elementsOfPF42 = rmdups [ P (F43 a b c)
                        | a <- elementsOfF4
                        , b <- elementsOfF4
                        , c <- elementsOfF4
                        , F43 a b c /= F43 Zero Zero Zero]
                          where
                            rmdups [] = []
                            rmdups (x:xs) | x `elem` xs = rmdups xs
                                          | otherwise = x : rmdups xs

-- | Create the equivalce class of a an element in PF43
equivalenceClass :: PF42 -> [PF42]
equivalenceClass (P v) = map (\k -> P (k ^* v)) [One, U, V]

equivalenzClassesOfPF42 = map equivalenceClass elementsOfPF42

prettyPrintMatrix :: (Foldable t, Show a) => t a -> IO ()
prettyPrintMatrix = mapM_ print
  where
    println = print . (++ "\n")
