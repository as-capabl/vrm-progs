{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module
    Data.GlTF.GlTF
where

import Conjct
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)

do
    scRoot <- liftIO $ getEnv "HSGLTF_SCHEMA_ROOT"
    if null scRoot
        then fail "Set HSGLTF_SCHEMA_ROOT environment variable"
        else return ()
    let stg = Conjct.defaultSchemaSetting (T.pack scRoot)
    Conjct.fromSchema
        stg
        "glTF.schema.json" 
