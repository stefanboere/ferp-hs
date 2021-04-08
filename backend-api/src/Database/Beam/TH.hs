{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module: Database.Beam.TH
Description: Template Haskell utilities for only the most tedeous instance declarations
This module only contains instance declarations, and only where many at once are required,
and for which only an explicit, tedious, method is available (StandaloneDeriving).
If you know a method to write these instances in a non-tedious way without TemplateHaskell,
please let me know. I'm interested in removing this module alltogether.

If you hate TemplateHaskell even more than I do, you can also copy all these instances of course.
-}
module Database.Beam.TH
  ( instances
  , instancesId
  , instancesT
  , instancesTFull
  , instancesQuery
  , instancesBody
  )
where

import qualified Data.Char                     as C
                                                ( isUpper )
import qualified Data.List                     as L
                                                ( dropWhileEnd )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Swagger                   ( ToParamSchema
                                                , ToSchema
                                                , declareNamedSchema
                                                , toParamSchema
                                                )
import           Data.Typeable                  ( Typeable )
import           Database.Beam                  ( Beamable
                                                , Columnar
                                                , Identity
                                                , PrimaryKey(..)
                                                , QExpr
                                                )
import           Database.Beam.API              ( Orderable )
import           Database.Beam.Backend.SQL      ( BeamSqlBackend )
import           Database.Beam.Deriving         ( BeamOrderBy(..) )
import           Database.Beam.Named            ( Full
                                                , Named
                                                )
import           Language.Haskell.TH
import           Servant.API                    ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Servant.Crud.Server.Deriving   ( JsonBody(..)
                                                , QueryType(..)
                                                )
import           Servant.Crud.Server.OrderBy    ( Selectors )
import           Servant.Crud.Server.QueryObject
                                                ( FromQueryText
                                                , NoContent
                                                , NoTypes
                                                , ToParams
                                                , ToQueryText
                                                )
import           Servant.Crud.Server.QueryOperator
                                                ( Filter )

-- | Creates instances for 'Eq', 'Show', 'FromJSON', 'ToJSON', 'FromQueryText'
-- 'ToQueryText', 'ToParams' and 'ToSchema' for the 'NameT Identity' and 'NameT Maybe'
-- Also creats relevant instances for 'NameT Filter'
instances :: Name -> DecsQ
instances n = concat <$> sequence
  [ instancesBody tId
  , instancesBody tMay
  , instancesQuery tQ
  , instancesOrderBy (conT n)
  ]
 where
  tId  = appT (conT n) (conT ''Identity)
  tMay = appT (conT n) (conT ''Maybe)
  tQ   = appT (conT n) (conT ''Filter)


-- | Like 'instances', but for types with an extra T
-- The first argument should either be 'Named' or 'Id', depending if you plan to
-- embed the full type or the Named type
-- brittany-disable-next-binding
instancesT' :: Name -> Name -> DecsQ
instancesT' n0 n = concat <$>
  sequence [ instancesBody  (t ''PrimaryKey ''Identity) -- HaskellValue / Put / Post
           , instancesBody  (t ''PrimaryKey ''Maybe)    -- Patch
           , instancesBody  (t n0 ''Identity)      -- Get
           , instancesQuery (t n0 ''Filter)      -- Filter object for Get
           , otherInstances
           , instancesOrderBy (appT (conT n) (conT n0))
           ]
    where
      otherInstances :: DecsQ
      otherInstances = [d|
          deriving instance Beamable ($(conT n) PrimaryKey)
          deriving instance Beamable $(appT (conT n) (conT n0))
        |]

      t :: Name -> Name -> TypeQ
      t n1 n2 = appT (appT (conT n) (conT n1)) (conT n2)

-- | Like 'instances', but for types with an extra T
instancesT :: Name -> DecsQ
instancesT = instancesT' ''Named

-- | Like 'instances', but for types with an extra T, with full types embedded
instancesTFull :: Name -> DecsQ
instancesTFull = instancesT' ''Full


-- | Regular instances like 'Eq', 'Show' and JSON related instances
-- brittany-disable-next-binding
instancesBody :: TypeQ -> DecsQ
instancesBody t =
    [d|
    deriving instance Eq $(t)
    deriving instance Show $(t)
    deriving instance ToSchema $(t)
    deriving via (JsonBody $(t)) instance FromJSON $(t)
    deriving via (JsonBody $(t)) instance ToJSON $(t)
    |]


-- | Creates instances for 'Eq', 'Show', 'FromJSON', 'ToJSON', 'FromQueryText'
-- 'ToQueryText', 'ToParams' and 'ToSchema' for the 'NameT Filter'
-- brittany-disable-next-binding
instancesQuery :: TypeQ -> DecsQ
instancesQuery t = [d|
    -- NameT Filter instances
    deriving instance Eq $(t)
    deriving instance Show $(t)
    deriving via QueryType $(t) instance FromQueryText $(t)
    deriving via QueryType $(t) instance ToQueryText $(t)
    deriving via QueryType $(t) instance ToParams NoTypes NoContent $(t)
    |]

-- brittany-disable-next-binding
instancesOrderBy :: TypeQ -> DecsQ
instancesOrderBy t = [d|
  deriving via BeamOrderBy ($(t) (QExpr be s)) instance
        (Typeable be, Typeable s, BeamSqlBackend be)
      => Selectors (Orderable be) ($(t) (QExpr be s))
   |]

-- | Creates relevant instances for 'NameId'
-- Takes the NameT type and the inner type of the primary key
-- brittany-disable-next-binding
instancesId :: Name -> Name -> DecsQ
instancesId n n0 = [d|
    -- Show instances
    deriving instance Show (PrimaryKey $(t) Identity)
    deriving instance Show (PrimaryKey $(t) Filter)
    deriving instance Show (PrimaryKey $(t) Maybe)

    -- Eq instances
    deriving instance Eq (PrimaryKey $(t) Identity)
    deriving instance Eq (PrimaryKey $(t) Filter)
    deriving instance Eq (PrimaryKey $(t) Maybe)

    -- JSON instances
    instance ToJSON (PrimaryKey $(t) Identity)
    instance ToJSON (PrimaryKey $(t) Maybe)
    instance FromJSON (PrimaryKey $(t) Identity)
    instance FromJSON (PrimaryKey $(t) Maybe)

    -- Other instances
    instance FromQueryText (Columnar f $(t0)) => FromQueryText (PrimaryKey $(t) f)
    instance ToQueryText   (Columnar f $(t0)) => ToQueryText   (PrimaryKey $(t) f)
    instance ToParams lang ftype (Columnar f $(t0)) => ToParams lang ftype (PrimaryKey $(t) f)

    instance ToSchema (Columnar f $(t0)) => ToSchema (PrimaryKey $(conT n) f) where
      declareNamedSchema p = declareNamedSchema (unProxy p)
        where
          unProxy :: proxy (PrimaryKey x f) -> proxy (Columnar f $(t0))
          unProxy _ = undefined

    instance ToParamSchema (Columnar f $(t0)) => ToParamSchema (PrimaryKey $(conT n) f) where
      toParamSchema p = toParamSchema (unProxy p)
        where
          unProxy :: proxy (PrimaryKey x f) -> proxy (Columnar f $(t0))
          unProxy _ = undefined

    instance FromHttpApiData (Columnar f $(t0)) => FromHttpApiData (PrimaryKey $(conT n) f) where
      parseQueryParam = fmap $(conE nameIdT) . parseQueryParam

    instance ToHttpApiData (Columnar f $(t0)) => ToHttpApiData (PrimaryKey $(conT n) f) where
      toQueryParam $(matchX) = toQueryParam x
    |]
    where
        t = conT n
        t0 = conT n0

        nameIdT = mkName (L.dropWhileEnd C.isUpper (nameBase n) ++ "Id" )
        matchX = conP nameIdT [varP (mkName "x") ]
