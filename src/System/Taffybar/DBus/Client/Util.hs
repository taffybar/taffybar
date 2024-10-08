{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module System.Taffybar.DBus.Client.Util where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif
import           DBus.Generation
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import qualified Data.Char as Char
import           Data.Coerce
import           Data.Maybe
import           Language.Haskell.TH
import           StatusNotifier.Util (getIntrospectionObjectFromFile)

deriveShowAndEQ :: [DerivClause]
deriveShowAndEQ =
  [DerivClause Nothing [ConT ''Eq, ConT ''Show]]

buildDataFromNameTypePairs :: Name -> [(Name, Type)] -> Dec
buildDataFromNameTypePairs name pairs =
  DataD [] name [] Nothing [RecC name (map mkVarBangType pairs)]
        deriveShowAndEQ
  where mkVarBangType (fieldName, fieldType) =
          (fieldName, Bang NoSourceUnpackedness NoSourceStrictness, fieldType)


standaloneDeriveEqShow :: Name -> [Dec]
standaloneDeriveEqShow _ = []

type GetTypeForName = String -> T.Type -> Maybe Type

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
               I.Interface { I.interfaceName = interfaceName
                           , I.interfaceProperties = properties
                           } = do
  let theRecordName =
        maybe (mkName $ map Char.toUpper $ filter Char.isLetter $ coerce interfaceName)
              mkName recordNameString
  let getPairFromProperty I.Property
                            { I.propertyName = propName
                            , I.propertyType = propType
                            } =
                            ( mkName $ prefix ++ propName
                            , fromMaybe (getArgType propType) $ getTypeForName propName propType
                            )
      getAllRecord =
        buildDataFromNameTypePairs
        theRecordName $ map getPairFromProperty properties
  return $ getAllRecord:standaloneDeriveEqShow theRecordName

generateClientFromFile :: RecordGenerationParams -> GenerationParams -> Bool -> FilePath -> Q [Dec]
generateClientFromFile recordGenerationParams params useObjectPath filepath = do
  object <- getIntrospectionObjectFromFile filepath "/"
  let interface = head $ I.objectInterfaces object
      actualObjectPath = I.objectPath object
      realParams =
        if useObjectPath
          then params {genObjectPath = Just actualObjectPath}
          else params
      (<++>) = liftA2 (++)
  generateGetAllRecord recordGenerationParams params interface <++>
    generateClient realParams interface <++>
    generateSignalsFromInterface realParams interface
