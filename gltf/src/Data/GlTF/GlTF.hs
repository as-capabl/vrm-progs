{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module
    Data.GlTF.GlTF
where

import Conjct
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Int
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Language.Haskell.TH

do
    scRoot <- liftIO $ getEnv "HSGLTF_SCHEMA_ROOT"
    if null scRoot
        then fail "Set HSGLTF_SCHEMA_ROOT environment variable"
        else return ()
    let onMember_custom _ fn mn _ _
            | fn == "sampler.schema.json" =
                if
                    | mn == "magFilter" || mn == "minFilter" || mn == "wrapS" || mn == "wrapT" -> Just $
                      do
                        nMem <- maybe (fail "memberName") return $ Conjct.memberNameDefault fn mn
                        return $ Conjct.mkFieldInfo nMem (ConT ''Maybe `AppT` ConT ''Int32) mn '(J..:)
                    | otherwise -> Nothing
            | otherwise = Nothing

        stgDef = Conjct.defaultSchemaSetting (T.pack scRoot)
        onM = onMember_custom : onMember stgDef
        stg = stgDef {
            onMember = onM
          }
    Conjct.fromSchema
        stg
        "glTF.schema.json" 
