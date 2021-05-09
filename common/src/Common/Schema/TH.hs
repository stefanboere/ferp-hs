{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module: Common.Schema.TH
Description: Template Haskell utilities for only the most tedeous instance declarations
This module only contains instance declarations, and only where many at once are required,
and for which only an explicit, tedious, method is available (StandaloneDeriving).
If you know a method to write these instances in a non-tedious way without TemplateHaskell,
please let me know. I'm interested in removing this module alltogether.

If you hate TemplateHaskell even more than I do, you can also copy all these instances of course.
-}
module Common.Schema.TH
  ( instances
  , instancesId
  , instancesT
  , instancesTFull
  , instancesBody
  )
where

import           Control.Lens                   ( makeLenses )
import qualified Data.Char                     as C
                                                ( isUpper )
import qualified Data.List                     as L
                                                ( dropWhileEnd )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Typeable                  ( Typeable )
import           Database.Beam                  ( Beamable
                                                , Columnar
                                                , PrimaryKey(..)
                                                , QExpr
                                                )
import           Database.Beam.API              ( Orderable )
import           Database.Beam.Backend.SQL      ( BeamSqlBackend )
import           Database.Beam.Deriving         ( BeamOrderBy(..) )
import           Database.Beam.Expand
import           GHC.Generics                   ( Rep )
import           Generic.Data
import           Language.Haskell.TH
import           Servant.API                    ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Servant.Crud.Deriving
import           Servant.Crud.OrderBy           ( Selectors )
import           Servant.Crud.QueryObject       ( FromQueryText
                                                , ToQueryText
                                                )
import           Servant.Crud.QueryOperator     ( Filter )

-- | Creates instances for 'Eq', 'Show', 'FromJSON', 'ToJSON', 'FromQueryText'
-- 'ToQueryText', 'ToParams' and 'ToSchema' for the 'NameT Identity' and 'NameT Maybe'
-- Also creats relevant instances for 'NameT Filter'
instances :: Name -> DecsQ
instances n = concat <$> sequence [instancesBody (conT n), makeLenses n]

-- | Like 'instances', but for types with an extra T
-- The first argument should either be 'Named' or 'Id', depending if you plan to
-- embed the full type or the Named type
-- brittany-disable-next-binding
instancesT' :: Name -> Name -> DecsQ
instancesT' n0 n = concat <$>
  sequence [ instancesBody  (t ''PrimaryKey) -- HaskellValue / Put / Post / Patch
           , instancesBody  (t n0)      -- Get / Filter object for Get
           , otherInstances
           , makeLenses n
           ]
    where
      otherInstances :: DecsQ
      otherInstances = [d|
          deriving instance Beamable ($(conT n) PrimaryKey)
          deriving instance Beamable $(appT (conT n) (conT n0))
        |]
      t :: Name -> TypeQ
      t n1 = appT (conT n) (conT n1)

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
    deriving via (Generically ($(t) f)) instance GEq ($(t) f) => Eq ($(t) f)
    deriving via (Generically ($(t) f)) instance GShow0 (Rep ($(t) f)) => Show ($(t) f)
    deriving via (Generically ($(t) f)) instance Semigroup (Rep ($(t) f) ()) => Semigroup ($(t) f)
    deriving via (Generically ($(t) f)) instance Monoid (Rep ($(t) f) ()) => Monoid ($(t) f)
    deriving via (JsonBody ($(t) f)) instance GToJSON ($(t) f) => ToJSON ($(t) f)
    deriving via (JsonBody ($(t) f)) instance GFromJSON ($(t) f) => FromJSON ($(t) f)

    -- NameT Filter instances
    deriving via (QueryType ($(t) Filter)) instance ToQueryText ($(t) Filter)
    deriving via (QueryType ($(t) Filter)) instance FromQueryText ($(t) Filter)

    deriving via BeamOrderBy ($(t) (QExpr be s)) instance
        (Typeable be, Typeable s, BeamSqlBackend be)
      => Selectors (Orderable be) ($(t) (QExpr be s))
    |]

-- | Creates relevant instances for 'NameId'
-- Takes the NameT type and the inner type of the primary key
-- brittany-disable-next-binding
instancesId :: Name -> Name -> DecsQ
instancesId n n0 = [d|
    -- Basic instances
    deriving instance Eq (Columnar f $(t0)) => Eq (PrimaryKey $(t) f)
    deriving instance Show (Columnar f $(t0)) => Show (PrimaryKey $(t) f)
    instance ToJSON (Columnar f $(t0)) => ToJSON (PrimaryKey $(t) f)
    instance FromJSON (Columnar f $(t0)) => FromJSON (PrimaryKey $(t) f)
    instance FromQueryText (Columnar f $(t0)) => FromQueryText (PrimaryKey $(t) f)
    instance ToQueryText   (Columnar f $(t0)) => ToQueryText   (PrimaryKey $(t) f)

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
