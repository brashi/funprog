module ExMatrix2x2
    ( matrix
    , zero
    , identity
    , rows
    , cols
    , getElem
    , transpose
    , det
    , isDiagonal
    , isTriangular
    , isLowerTriangular
    , isUpperTriangular
    , singular
    , invertible
    , inverse
    ) where

type Number = Double
type Row = [Number]
type Col = [Number]

data Matrix2x2 = Matrix2x2 {row1::Row, row2::Row, col1::Col, col2::Col}

mat1 = Matrix2x2 [1, 2] [3, 4] [1, 3] [2, 4]

mat2 = Matrix2x2 [1, 2] [3, 1] [1, 3] [2, 4]
instance Show Matrix2x2 where
    show mat = show (row1 mat) ++ "\n" ++ show (row2 mat)

instance Eq Matrix2x2 where
    (==)  matX matY = row1 matX == row1 matY && row2 matX == row2 matY

instance Num Matrix2x2 where
    (+) matX matY = Matrix2x2 (row1 matX ++ row1 matY) (row2 matX ++ row2 matY) (col1 matX ++ col1 matY) (col2 matX ++ col2 matY)
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

-- matrix a b c d should create the matrix
-- ( a c )
-- ( b d )
matrix :: Number -> Number -> Number -> Number -> Matrix2x2
matrix = undefined

zero :: Matrix2x2
zero = undefined

identity :: Matrix2x2
identity = undefined

rows :: Matrix2x2 -> [Row]
rows = undefined

cols :: Matrix2x2 -> [Col]
cols = undefined

getElem :: (Int,Int) -> Matrix2x2 -> Number
getElem = undefined

transpose :: Matrix2x2 -> Matrix2x2
transpose = undefined

det :: Matrix2x2 -> Number
det = undefined

isDiagonal :: Matrix2x2 -> Bool
isDiagonal = undefined

isTriangular :: Matrix2x2 -> Bool
isTriangular = undefined

isLowerTriangular :: Matrix2x2 -> Bool
isLowerTriangular = undefined

isUpperTriangular :: Matrix2x2 -> Bool
isUpperTriangular = undefined

singular :: Matrix2x2 -> Bool
singular = undefined

invertible :: Matrix2x2 -> Bool
invertible = not . singular

inverse :: Matrix2x2 -> Matrix2x2
inverse = undefined

