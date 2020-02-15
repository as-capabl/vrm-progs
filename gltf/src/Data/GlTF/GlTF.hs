{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Data.GlTF.GlTF
where

import qualified CONJCT
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Int
import Data.Word
import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard)
import System.Environment (getEnv)
import Language.Haskell.TH
import TH.RelativePaths

import Data.GlTF.Prim

do
    scRoot <- pathRelativeToCabalPackage "schema"
    let onMember_sampler arg ms@(CONJCT.getModuleSummary -> CONJCT.ModuleSummary{..}) mn _ =
          do
            guard $ msSchemaFile == "sampler.schema.json"
            if
                | mn == "magFilter" || mn == "minFilter" || mn == "wrapS" || mn == "wrapT" -> Just $
                  do
                    nMem <- CONJCT.memberName (CONJCT.getSchemaSetting arg) arg ms mn
                    return $ CONJCT.mkFieldInfo nMem (ConT ''Maybe `AppT` ConT ''Int32) mn '(J..:)
                | otherwise -> Nothing

        onMember_accessor arg ms@(CONJCT.getModuleSummary -> CONJCT.ModuleSummary{..}) mn _ =
          do
            guard $ msSchemaFile == "accessor.schema.json"
            if
                | mn == "componentType" -> Just $ 
                  do
                    nMem <- CONJCT.memberName (CONJCT.getSchemaSetting arg) arg ms mn
                    return $ CONJCT.mkFieldInfo nMem (ConT ''Word32) mn '(J..:)
                | mn == "type" -> Just $
                  do
                    nMem <- CONJCT.callSchemaSetting CONJCT.memberName arg ms mn
                    return $ CONJCT.mkFieldInfo nMem (ConT ''AccType) mn '(J..:)
                | otherwise -> Nothing
    
        onType_float _ o =
          do
            guard $ o `CONJCT.isType` "number"
            return $ return $ ConT ''Float

        stgDef = CONJCT.defaultSchemaSetting (T.pack scRoot)
        onM = onMember_sampler : onMember_accessor : CONJCT.onMember stgDef
        onT = onType_float : CONJCT.onType stgDef
        stg = CONJCT.SimpleSchemaSetting $
            stgDef {
                CONJCT.onMember = onM,
                CONJCT.onType = onT
              }

    CONJCT.fromSchema stg "glTF.schema.json" 
