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
  , instancesBody
  )
where

import           Data.ByteString                ( ByteString )
import qualified Data.Csv                      as Csv
import           Data.Swagger                   ( ToParamSchema
                                                , ToSchema
                                                , declareNamedSchema
                                                , toParamSchema
                                                )
import           Database.Beam                  ( Columnar
                                                , Identity
                                                , PrimaryKey(..)
                                                )
import           Database.Beam.Named            ( Full
                                                , Named
                                                )
import           GHC.Generics                   ( Rep )
import           Language.Haskell.TH
import           Servant.Crud.Server.Deriving
import           Servant.Crud.Server.QueryObject
                                                ( ToParams
                                                , NoContent
                                                , NoTypes
                                                )
import           Servant.Crud.Server.QueryOperator
                                                ( Filter
                                                , MaybeLast
                                                )

-- | Creates instances for 'Eq', 'Show', 'FromJSON', 'ToJSON', 'FromQueryText'
-- 'ToQueryText', 'ToParams' and 'ToSchema' for the 'NameT Identity' and 'NameT Maybe'
-- Also creats relevant instances for 'NameT Filter'
instances :: Name -> DecsQ
instances n =
  concat <$> sequence [instancesBody (conT n), instancesSchema (conT n)]

-- | Like 'instances', but for types with an extra T
-- The first argument should either be 'Named' or 'Id', depending if you plan to
-- embed the full type or the Named type
-- brittany-disable-next-binding
instancesT' :: Name -> Name -> DecsQ
instancesT' n0 n = concat <$>
  sequence [ instancesBody  (t ''PrimaryKey) -- HaskellValue / Put / Post / Patch
           , instancesBody  (t n0)      -- Get / Filter object for Get
           , instancesSchema (t ''PrimaryKey)
           ]
    where
      t :: Name -> TypeQ
      t n1 = appT (conT n) (conT n1)

-- | Like 'instances', but for types with an extra T
instancesT :: Name -> DecsQ
instancesT = instancesT' ''Named

-- | Like 'instances', but for types with an extra T, with full types embedded
instancesTFull :: Name -> DecsQ
instancesTFull = instancesT' ''Full

-- | Swagger schema instances
-- brittany-disable-next-binding
instancesSchema :: TypeQ -> DecsQ
instancesSchema t =
  [d|
  deriving instance ToSchema ($(t) Identity)
  deriving instance ToSchema ($(t) MaybeLast)
  |]

-- | Regular instances like 'Eq', 'Show' and JSON related instances
-- brittany-disable-next-binding
instancesBody :: TypeQ -> DecsQ
instancesBody t =
    [d|
    deriving instance Csv.GToRecord (Rep ($(t) f)) (ByteString, ByteString) => Csv.ToNamedRecord ($(t) f)
    deriving instance Csv.GFromNamedRecord (Rep ($(t) f))  => Csv.FromNamedRecord ($(t) f)
    deriving instance Csv.GToNamedRecordHeader (Rep ($(t) f))  => Csv.DefaultOrdered ($(t) f)

    -- NameT Filter instances
    deriving via (QueryType ($(t) Filter)) instance ToParams NoTypes NoContent ($(t) Filter)
    |]

-- | Creates relevant instances for 'NameId'
-- Takes the NameT type and the inner type of the primary key
-- brittany-disable-next-binding
instancesId :: Name -> Name -> DecsQ
instancesId n n0 = [d|
    -- Basic instances
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
    |]
    where
        t = conT n
        t0 = conT n0
