{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  ()
where

import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           Servant.Crud.Server.QueryOperator
                                                ( DefaultFilters
                                                , OrdFilter
                                                )
-- * Date orphans

type instance DefaultFilters Day = OrdFilter
type instance DefaultFilters UTCTime = OrdFilter

