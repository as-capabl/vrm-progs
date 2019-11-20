{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module
    Data.GlTF.Prim
      (
        AccType(..)
      )
where

import Data.Aeson
import qualified Data.Text as T

data AccType = AccScalar | AccVec2 | AccVec3 | AccVec4 | AccMat2 | AccMat3 | AccMat4 | AccUnk T.Text
  deriving (Eq, Show)

instance FromJSON AccType
  where
    parseJSON = withText "AccType" $ \s -> return $
        if  | s == "SCALAR" -> AccScalar
            | s == "VEC2" -> AccVec2
            | s == "VEC3" -> AccVec3
            | s == "VEC4" -> AccVec4
            | s == "MAT2" -> AccMat2
            | s == "MAT3" -> AccMat3
            | s == "MAT4" -> AccMat4
            | otherwise -> AccUnk s
