{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module
    Data.GlTF.GlTF
where

import qualified CONJCT
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Int
import Data.Word
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Language.Haskell.TH
import TH.RelativePaths

import Data.GlTF.Prim

do
    scRoot <- pathRelativeToCabalPackage "schema"
    let onMember_custom _ fn mn _ _
            | fn == "sampler.schema.json" =
                if
                    | mn == "magFilter" || mn == "minFilter" || mn == "wrapS" || mn == "wrapT" -> Just $
                      do
                        nMem <- maybe (fail "memberName") return $ CONJCT.memberNameDefault fn mn
                        return $ CONJCT.mkFieldInfo nMem (ConT ''Maybe `AppT` ConT ''Int32) mn '(J..:)
                    | otherwise -> Nothing
            | fn == "accessor.schema.json" =
                if
                    | mn == "componentType" -> Just $ 
                      do
                        nMem <- maybe (fail "memberName") return $ CONJCT.memberNameDefault fn mn
                        return $ CONJCT.mkFieldInfo nMem (ConT ''Word32) mn '(J..:)
                    | mn == "type" -> Just $
                      do
                        nMem <- maybe (fail "memberName") return $ CONJCT.memberNameDefault fn mn
                        return $ CONJCT.mkFieldInfo nMem (ConT ''AccType) mn '(J..:)
                    | otherwise -> Nothing
            | otherwise = Nothing
        onType_custom _ o =
          do
            CONJCT.checkType o "number"
            return $ return $ ConT ''Float

        stgDef = CONJCT.defaultSchemaSetting (T.pack scRoot)
        onM = onMember_custom : CONJCT.onMember stgDef
        onT = onType_custom : CONJCT.onType stgDef
        stg = stgDef {
            CONJCT.onMember = onM,
            CONJCT.onType = onT
          }
    CONJCT.fromSchema
        stg
        "glTF.schema.json" 
