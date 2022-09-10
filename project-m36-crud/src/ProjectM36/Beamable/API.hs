{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ProjectM36.Beamable.API
  ( -- * Types
    GetList
  , GetListLabels
  , Get_
  , Put_
  , Patch_
  , Delete_
  , DeleteList_
  , Post_
  , PostList
  , CaptureId
  , View
  -- * Re-exports
  , Filter
  , View'(..)
  , Page(..)
  , AttrName
  ) where

import           Data.Functor.Const             ( Const )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Monoid                    ( Last )
import           ProjectM36.Base                ( AttributeName )
import           ProjectM36.Beamable.Class
import           ProjectM36.Beamable.Expand
import           ProjectM36.Beamable.OrderBy
import           Servant.API
import           Servant.Crud.API
import           Servant.Crud.Headers           ( ExceptLimited(..)
                                                , PathInfo
                                                )
import           Servant.Crud.QueryObject       ( QObj )
import           Servant.Crud.QueryOperator     ( Filter )

type AttrName = Const AttributeName

-- | Contains ordering, filtering and pageination info. This particular type works
-- nice with 'setView'. You can use this type with 'QueryObject' to allow the client
-- to specify ordering, filtering and pageination info in the request using request
-- parameters.
--
-- Then, in the handler, simply use 'setView' to alter the query to match this requested view.
type View t = View' Orderable (t AttrName) (t Filter)

-- | Endpoint returning a list of @t Identity@ based on a @View@ in the request query parameters
--
-- E.g. the type of the @\/books@ endpoint.
type GetList t = PathInfo :> QObj (View t) :> GetList' (t Identity)

-- | Subroute compared to @GetList@ which only returns the primary keys and labels
type GetListLabels t
  = "labels"
  :> PathInfo
  :> QObj (View t)
  :> QueryParam "around" (PrimaryKey (BaseTable t) Identity) -- TODO move this parameter into View' itself
  :> Get '[JSON] (GetListHeaders (Named (BaseTable t) Identity))

-- | Regular get requests
type Get_ t = CaptureId (BaseTable t) :> Get' (t Identity)

-- | Regular patch requests
type Patch_ t = CaptureId t :> Req' (t Last) :> PatchNoContent

-- | Regular put requests
type Put_ t = CaptureId t :> Req' (t Identity) :> PutNoContent

-- | Regular delete requests
type Delete_ t = CaptureId t :> DeleteNoContent

-- | Delete list requests
type DeleteList_ t
  = Req' (ExceptLimited [PrimaryKey (BaseTable t) Identity])
  :> QObj (t Filter) :> Delete '[JSON] [PrimaryKey (BaseTable t) Identity]

-- | Regular post requests
type Post_ t
  = PathInfo :> "0" :> Req' (t Identity) :> Post_' (PrimaryKey t Identity)

-- | Posting many records
type PostList t
  = ReqCSV' [t Identity] :> PostCreated '[JSON] [PrimaryKey t Identity]

-- | Id in the request path
type CaptureId t = Capture "id" (PrimaryKey t Identity)

