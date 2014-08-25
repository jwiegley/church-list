{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing
                -fno-warn-missing-signatures
                -fno-warn-unused-binds
                -fno-warn-orphans #-}

module Main where

import           Data.Eq
import           Data.Int
import qualified Data.List as L
import           Data.Church.List
import           Test.QuickCheck

prop_length (xs :: [Int]) = length (fromList xs) == L.length xs

main = do
    let q = quickCheck
    q prop_length
