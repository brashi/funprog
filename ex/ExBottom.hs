module ExBottom where

{- For each of the types we have encountered so far
 - (1) how many values does this type have?
 - (2) describe all its values.
 - Create contexts to demonstrate your claims
 - regarding which values are really distinct within
 - each type.
 -}

-- Bool
-- Bool has 3 values:
-- False, True, bottom
-- To show they are distinct with v :: Bool:
-- define:
bottomBool :: Bool -> Int
bottomBool False = 0
bottomBool True  = 1

bottom :: a
bottom = bottom
-- check: bottomBool v
-- EU fiz...
-- *ExBottom> bottomBool (v :: Bool)
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries\base\GHC\Err.hs:79:14 in base:GHC.Err
--   undefined, called at ex\ExBottom.hs:20:5 in main:ExBottom



-- Any of (choose one): Int, Char, Double

-- Integer

-- Nat

-- Box α

-- Tuples: (α,β)

-- List α

