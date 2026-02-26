{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module DBusMenu.Client.Util
  ( RecordGenerationParams(..)
  , GetTypeForName
  , defaultRecordGenerationParams
  , generateClientFromFile
  ) where

import Control.Monad (forM)
import DBus (ObjectPath)
import DBus.Generation
import qualified DBus.Internal.Types as DBusTypes
import qualified DBus.Introspection as I
import qualified Data.Char as Char
import qualified Data.Coerce as Coerce
import qualified Data.Maybe as Maybe
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)

type GetTypeForName = String -> DBusTypes.Type -> Maybe Type

data RecordGenerationParams = RecordGenerationParams
  { recordName :: Maybe String
  , recordPrefix :: String
  , recordTypeForName :: GetTypeForName
  }

defaultRecordGenerationParams :: RecordGenerationParams
defaultRecordGenerationParams = RecordGenerationParams
  { recordName = Nothing
  , recordPrefix = "_"
  , recordTypeForName = const $ const Nothing
  }

deriveShowAndEQ :: [DerivClause]
deriveShowAndEQ =
  [DerivClause Nothing [ConT ''Eq, ConT ''Show]]

buildDataFromNameTypePairs :: Name -> [(Name, Type)] -> Dec
buildDataFromNameTypePairs name pairs =
  DataD [] name [] Nothing [RecC name (map mkVarBangType pairs)] deriveShowAndEQ
  where
    mkVarBangType (fieldName, fieldType) =
      ( fieldName
      , Bang NoSourceUnpackedness NoSourceStrictness
      , fieldType
      )

generateGetAllRecord
  :: RecordGenerationParams
  -> GenerationParams
  -> I.Interface
  -> Q [Dec]
generateGetAllRecord
  RecordGenerationParams
    { recordName = recordNameString
    , recordPrefix = prefix
    , recordTypeForName = getTypeForName
    }
  GenerationParams { getTHType = getArgType }
  I.Interface
    { I.interfaceName = interfaceName
    , I.interfaceProperties = properties
    } = do
  let theRecordName =
        mkName $
          maybe
            (map Char.toUpper $ filter Char.isLetter $ Coerce.coerce interfaceName)
            id
            recordNameString
      getPairFromProperty
        I.Property { I.propertyName = propName, I.propertyType = propType } =
          ( mkName $ prefix ++ propName
          , Maybe.fromMaybe (getArgType propType) $
              getTypeForName propName propType
          )
      getAllRecord =
        buildDataFromNameTypePairs theRecordName $
          map getPairFromProperty properties
  pure [getAllRecord]

getIntrospectionObjectFromFile :: FilePath -> ObjectPath -> Q I.Object
getIntrospectionObjectFromFile filepath path = do
  realPath <- makeRelativeToProject filepath
  addDependentFile realPath
  xml <- runIO (TIO.readFile realPath)
  case I.parseXML path xml of
    Nothing -> fail $ "Failed to parse DBus introspection XML: " <> filepath
    Just obj -> pure obj

generateClientFromFile
  :: RecordGenerationParams
  -> GenerationParams
  -> Bool
  -> FilePath
  -> Q [Dec]
generateClientFromFile recordGenerationParams params useObjectPath filepath = do
  obj <- getIntrospectionObjectFromFile filepath "/"
  let actualObjectPath = I.objectPath obj
      realParams =
        if useObjectPath
          then params { genObjectPath = Just actualObjectPath }
          else params
      (<++>) = liftA2 (++)
  fmap concat $ forM (I.objectInterfaces obj) $ \interface -> do
    generateGetAllRecord recordGenerationParams params interface <++>
      generateClient realParams interface <++>
      generateSignalsFromInterface realParams interface
