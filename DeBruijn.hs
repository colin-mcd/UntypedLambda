module DeBruijin where

-- De Bruijn-indexed term
data DBTm =
    DBVar Int
  | DBApp DBTm DBTm
  | DBLam DBTm
