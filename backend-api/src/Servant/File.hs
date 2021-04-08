{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Servant.File
  ( File
  , ResizedImage
  , defaultFileRoutes
  , FileRoutes(..)
  , SourceFormats
  , ImageRoutes
  , VerbRaw(..)
  , GetRaw
  , Post_
  , Delete_
  , Put_
  )
where

import           Codec.Picture
import           Codec.Picture.Extra            ( crop
                                                , scaleBilinear
                                                )
import           Codec.Picture.Metadata         ( SourceFormat(..) )
import           Codec.Picture.Saving
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , when
                                                , unless
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BL
                                                ( ByteString
                                                , fromStrict
                                                , toStrict
                                                , writeFile
                                                )
import           Data.Char                      ( toLower )
import           Data.List                      ( isInfixOf )
import qualified Data.List.NonEmpty            as NE
                                                ( toList )
import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
                                                ( toLower
                                                , unpack
                                                )
import qualified Data.Text.Encoding            as Text
                                                ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.Swagger                   ( NamedSchema(..)
                                                , Param(..)
                                                , ParamAnySchema(..)
                                                , ParamLocation(..)
                                                , ParamOtherSchema(..)
                                                , SwaggerType(..)
                                                , ToSchema(..)
                                                , type_
                                                )
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Lens.Micro                     ( (%~)
                                                , (.~)
                                                , (?~)
                                                , (&)
                                                )
import           Network.HTTP.Media             ( (//)
                                                , MediaType
                                                , mapContentMedia
                                                )
import           Servant
import           Servant.API.ContentTypes       ( AllCTUnrender(..)
                                                , AllMime(..)
                                                , AllMimeUnrender(..)
                                                )
import           Servant.API.Generic            ( Generic
                                                , (:-)
                                                )
import           Servant.AccessControl          ( AddSetHeaderApi
                                                , AddSetHeaderApiVerb
                                                )
import           Servant.Aeson.Internal         ( HasGenericSpecs(..) )
import           Servant.Client.Core            ( HasClient(..)
                                                , RunClient
                                                )
import           Servant.Crud.Server.API        ( Post_' )
import           Servant.Crud.Server.Headers    ( PathInfo(..)
                                                , hLocation
                                                )
import           Servant.Docs                   ( DocCapture(..)
                                                , HasDocs(..)
                                                , ToCapture(..)
                                                , ToSample(..)
                                                )
import           Servant.Ekg                    ( HasEndpoint(..) )
import           Servant.Foreign                ( HasForeign(..)
                                                , HasForeignType(..)
                                                , Req(..)
                                                , reqFuncName
                                                , reqMethod
                                                , reqReturnType
                                                )
import           Servant.Foreign.Internal       ( _FunctionName )
import           Servant.Multipart
import           Servant.QuickCheck.Internal.HasGenRequest
                                                ( HasGenRequest(..) )
import           Servant.Server.Generic         ( AsServerT )
import           Servant.Swagger                ( HasSwagger(..) )
import           Servant.Swagger.Internal
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , removeFile
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                , takeExtension
                                                )
import           System.Random                  ( newStdGen
                                                , randomRs
                                                )

-- | A File name with extension, but without directories
type FileName = String

-- | A file name in the route
type CaptureFileName = Capture "filename" FileName

instance ToCapture CaptureFileName where
  toCapture _ = DocCapture "filename" "The name of the requested file"

-- | Replaces a file in the directory with a certian name
-- brittany-disable-next-binding
type Put_ ctypes a
  = CaptureFileName
      :>
      MultipartForm
      Mem
      (File ctypes a)
      :>
      PutNoContent
      '[JSON]
      ()

-- | Deletes a file in the directory with a certain name
type Delete_ = CaptureFileName :> DeleteNoContent '[JSON] ()

-- | Add a new file
type Post_ ctypes a = PathInfo :> MultipartForm Mem (File ctypes a) :> Post_'

type ImageRoutes size = FileRoutes SourceFormats (ResizedImage size)

-- | Defines a common set of routes for implementing file uploads
data FileRoutes ctypes a route = FileRoutes
  { _put    :: route :- Put_ ctypes a
  , _delete :: route :- Delete_
  , _post   :: route :- Post_ ctypes a
  , _get    :: route :- GetRaw ctypes a -- This needs to be last, otherwise we get 405 errors
  }
  deriving Generic

{-| A default implementation of a file upload api
-}
defaultFileRoutes
  :: (MonadError ServerError m, MonadIO m)
  => FilePath
  -> FileRoutes ctypes a (AsServerT m)
defaultFileRoutes basePath = FileRoutes
  { _get    = serveDirectoryWebApp basePath
  , _put    = \fname (File name mimeType file) -> do
    when
      (  map toLower (takeExtension fname)
      /= map toLower (takeExtension (Text.unpack name))
      )
      (throwError err400
        { errBody =
          "Changing the image type is not yet supported. Try deleting and uploading a new file."
        }
      )
    path <- checkFileName basePath fname
    liftIO $ BL.writeFile path (mimeRender mimeType file)
  , _delete = \fname -> do
                path <- checkFileName basePath fname
                liftIO $ removeFile path
  , _post   = \pathinfo (File name mimeType file) -> do
    liftIO $ createDirectoryIfMissing True basePath
    fname <- liftIO getRandomName
    let ext  = map toLower . takeExtension . Text.unpack $ name
    let path = basePath </> fname <.> ext
    exists <- liftIO $ doesFileExist path
    when
      exists
      (throwError $ err500 { errBody = "File to be created already exists" })
    _ <- liftIO $ BL.writeFile path (mimeRender mimeType file)
    pure $ hLocation pathinfo (fname <.> ext) ()
  }

{-| Does some checks and returns the right file name
-}
checkFileName
  :: (MonadError ServerError m, MonadIO m) => FilePath -> String -> m FilePath
checkFileName basePath fname = do
  let fileName = map toLower fname
  when
    (  any (`notElem` ('.' : ['a' .. 'z'] ++ ['0' .. '9'])) fileName
    || isInfixOf ".." fileName
    )
    (throwError (err400 { errBody = "Invalid file name" }))
  let path = basePath </> fileName
  exists <- liftIO $ doesFileExist path
  unless
    exists
    (throwError (err404 { errBody = "Could not find the file specified" }))
  pure path

getRandomName :: IO String
getRandomName = take 10 . randomRs ('a', 'z') <$> newStdGen


-- * GetFiles

-- | Basically just the raw response, but with extra metadata attached to it
-- which allows for nice documentation
-- It is really actually an unsafe version of Verb. The implementer must make sure
-- that the attached metadata is satisfied
newtype VerbRaw method status ctypes a = VerbRaw { unVerbRaw :: Raw }

type GetRaw = VerbRaw 'GET 200

instance HasServer (VerbRaw method status ctypes a) context where
  type ServerT (VerbRaw method status ctypes a) m = ServerT Raw m

  route _ = route (Proxy :: Proxy Raw)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy Raw)

instance HasDocs (Verb method status cs a)
    => HasDocs (VerbRaw method status cs a) where
  docsFor _ = docsFor (Proxy :: Proxy (Verb method status cs a))

instance HasSwagger (Verb method status cs a)
   => HasSwagger (VerbRaw method status cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Verb method status cs a))

instance HasEndpoint (Verb method status cs a)
  => HasEndpoint (VerbRaw method status cs a) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy (Verb method status cs a))
  enumerateEndpoints _ =
    enumerateEndpoints (Proxy :: Proxy (Verb method status cs a))

instance (RunClient m, HasClient m (Verb method status cs a))
  => HasClient m (VerbRaw method status cs a) where
  type Client m (VerbRaw method status cs a) = Client
    m
    (Verb method status cs a)

  clientWithRoute pm _ =
    clientWithRoute pm (Proxy :: Proxy (Verb method status cs a))

  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy :: Proxy (Verb method status cs a))

instance HasGenRequest (Verb method status cs a)
  => HasGenRequest (VerbRaw method status cs a) where
  genRequest _ = genRequest (Proxy :: Proxy (Verb method status cs a))

instance (HasForeignType lang ftype a, ReflectMethod method)
  => HasForeign lang ftype (VerbRaw method status list a) where
  type Foreign ftype (VerbRaw method status list a) = Req ftype

  foreignFor lang Proxy Proxy req =
    req
      &  reqFuncName
      .  _FunctionName
      %~ (methodLC :)
      &  reqMethod
      .~ method
      &  reqReturnType
      ?~ retType
   where
    retType  = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)
    method   = reflectMethod (Proxy :: Proxy method)
    methodLC = Text.toLower $ Text.decodeUtf8 method


instance HasGenericSpecs (Verb method status cs a)
  => HasGenericSpecs (VerbRaw method status cs a) where
  collectRoundtripSpecs settings _ =
    collectRoundtripSpecs settings (Proxy :: Proxy (Verb method status cs a))

type instance AddSetHeaderApi sym t (VerbRaw method stat ctyps a)
  = VerbRaw method stat ctyps (AddSetHeaderApiVerb sym t a)

-- * Accept instances

-- ** Juicy pixels source formats

class KnownSourceFormat format where
  sourceFormatVal :: Proxy format -> SourceFormat

instance KnownSourceFormat 'SourceBitmap where
  sourceFormatVal _ = SourceBitmap
instance KnownSourceFormat 'SourceGif where
  sourceFormatVal _ = SourceGif
instance KnownSourceFormat 'SourceJpeg where
  sourceFormatVal _ = SourceJpeg
instance KnownSourceFormat 'SourcePng where
  sourceFormatVal _ = SourcePng
instance KnownSourceFormat 'SourceTiff where
  sourceFormatVal _ = SourceTiff
instance KnownSourceFormat 'SourceHDR where
  sourceFormatVal _ = SourceHDR
instance KnownSourceFormat 'SourceTGA where
  sourceFormatVal _ = SourceTGA

-- | We know what content types belong to each SourceFormat, of course
instance {-# OVERLAPPABLE #-} KnownSourceFormat format => Accept format where
  contentType _ = toMediaType $ sourceFormatVal (Proxy :: Proxy format)

-- | JuicyPixels provides us the encoders
instance KnownSourceFormat format => MimeRender format DynamicImage where
  mimeRender _ =
    either error id . encodeImage (sourceFormatVal (Proxy :: Proxy format))

-- | JuicyPixels provides us the decoders
instance KnownSourceFormat format => MimeUnrender format DynamicImage where
  mimeUnrender _ =
    decodeImage' (sourceFormatVal (Proxy :: Proxy format)) . BL.toStrict

-- | Now 'SourceBitmap has /Kind/ 'SourceFormat' but we need it to have the more general kind '*'
-- Therefore we basically create synonyms for each such type, use these if you
-- get type errors like Couldn't match expected kind * with kind 'SourceFormat'
data BITMAP
data GIF
data JPEG
data PNG
data TIFF
data HDR
data TGA

-- | Commonly we would want to have them all
type SourceFormats = '[BITMAP, GIF, JPEG, PNG, TIFF, HDR, TGA]

-- | Synonym instances
instance KnownSourceFormat BITMAP where
  sourceFormatVal _ = SourceBitmap
instance KnownSourceFormat GIF where
  sourceFormatVal _ = SourceGif
instance KnownSourceFormat JPEG where
  sourceFormatVal _ = SourceJpeg
instance KnownSourceFormat PNG where
  sourceFormatVal _ = SourcePng
instance KnownSourceFormat TIFF where
  sourceFormatVal _ = SourceTiff
instance KnownSourceFormat HDR where
  sourceFormatVal _ = SourceHDR
instance KnownSourceFormat TGA where
  sourceFormatVal _ = SourceTGA


-- ** Resized images

{-| A dynamic image with the source format tagged onto it, and also the maximal size
-}
newtype ResizedImage size = ResizedImage { unResizedImage :: DynamicImage }

instance KnownSourceFormat format => MimeRender format (ResizedImage size) where
  mimeRender _ =
-- Forget about the source format, we can also deliver any other requested format
    either error id
      . encodeImage (sourceFormatVal (Proxy :: Proxy format))
      . unResizedImage

instance (KnownSourceFormat format, KnownSize size) => MimeUnrender format (ResizedImage size) where
  mimeUnrender _ bs = do
    let f = sourceFormatVal (Proxy :: Proxy format)
    img        <- decodeImage' f . BL.toStrict $ bs
    resizedImg <- resized img
    pure $ ResizedImage resizedImg
   where
    dims = sizeVal (Proxy :: Proxy size)

    resized :: DynamicImage -> Either String DynamicImage
    resized = maybe (Left "Could not resize image") Right . resizeAndCrop' dims

instance ToSample (ResizedImage size) where
  toSamples _ = [] -- Image examples do not go well in the documentation, so ignore these

instance ToSchema (ResizedImage size) where
  declareNamedSchema _ = pure $ NamedSchema (Just "An image") mempty

class KnownSize size where
  sizeVal :: Proxy size -> (Maybe Int, Maybe Int)

instance (KnownNat x, KnownNat y) => KnownSize '( 'Just x, 'Just y) where
  sizeVal _ =
    ( Just . fromInteger $ natVal (Proxy :: Proxy x)
    , Just . fromInteger $ natVal (Proxy :: Proxy y)
    )

instance (KnownNat x) => KnownSize '( 'Just x, 'Nothing) where
  sizeVal _ = (Just . fromInteger $ natVal (Proxy :: Proxy x), Nothing)

instance (KnownNat y) => KnownSize '( 'Nothing, 'Just y) where
  sizeVal _ = (Nothing, Just . fromInteger $ natVal (Proxy :: Proxy y))

instance KnownSize '( 'Nothing, 'Nothing) where
  sizeVal _ = (Nothing, Nothing)

-- * MultipartForm orphans

data MultipartFormData

instance Accept MultipartFormData where
  contentType _ = "multipart" // "form-data"

instance HasSwagger sub => HasSwagger (MultipartForm tag a :> sub) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy sub)
      & addParam param
      & addConsumes
          (allContentType (Proxy :: Proxy '[MultipartFormData, FormUrlEncoded]))
      & addDefaultResponse400 tname
   where
    tname = "body"

    ref =
      ParamOtherSchema ParamFormData (Just False)
        $  mempty
        &  type_
        ?~ SwaggerFile

    param = Param tname Nothing (Just True) (ParamOther ref)

instance HasEndpoint (sub :: *) => HasEndpoint (MultipartForm tag a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)


-- * The 'File' type

-- | A file with MimeType 'ctype' and Haskell content type 'a'
data File ctypes a = forall ctype . MimeRender ctype a => File Text
                                                               (Proxy ctype)
                                                               a

instance (ToSample a, Accept ct, MimeRender ct a) => ToMultipartSample Mem (File (ct ': cts) a) where
  toMultipartSamples _ = map mkSample (toSamples (Proxy :: Proxy a))
   where
    mkSample :: (Text, a) -> (Text, MultipartData Mem)
    mkSample (desc, x) =
      ( desc
      , MultipartData
        []
        [ FileData "file"
                   "filename"
                   (pack . show $ contentType ct)
                   (mimeRender ct x)
        ]
      )

    ct :: Proxy ct
    ct = Proxy

instance (AllMimeUnrender ctypes a, AllMimeAccept ctypes a) => FromMultipart Mem (File ctypes a) where
  fromMultipart form = listToMaybe (files form) >>= decodeSomeFileMem

instance (AllMimeUnrender ctypes a, AllMimeAccept ctypes a) => FromMultipart Mem [File ctypes a] where
  fromMultipart form = mapM decodeSomeFileMem (files form)

decodeSomeFileMem
  :: forall ctypes a
   . (AllMimeUnrender ctypes a, AllMimeAccept ctypes a)
  => FileData Mem
  -> Maybe (File ctypes a)
decodeSomeFileMem fd = do
  decoder   <- canHandleCTypeH (Proxy :: Proxy ctypes) (BL.fromStrict bsHeader)
  somectype <- acceptCtype (Proxy :: Proxy ctypes) bsHeader
  eitherToMaybe $ mkFile (fdFileName fd) somectype <$> decoder (fdPayload fd)
 where
  bsHeader = Text.encodeUtf8 $ fdFileCType fd

  mkFile :: Text -> SomeContentType a -> a -> File ctypes a
  mkFile fname (SomeContentType proxy) = File fname proxy

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe x = case x of
  Right y -> Just y
  Left  _ -> Nothing

data SomeContentType a
  = forall ctype
  . (Accept ctype, MimeRender ctype a) =>
    SomeContentType (Proxy ctype)

class AllMime list => AllMimeAccept list a where
  allMimeAccept :: Proxy list -> [(MediaType, SomeContentType a)]

instance AllMimeAccept '[] a where
  allMimeAccept _ = []

instance (Accept ctype, MimeRender ctype a, AllMimeAccept ctypes a)
  => AllMimeAccept (ctype ': ctypes) a where
  allMimeAccept _ = map (, SomeContentType (Proxy :: Proxy ctype)) ctypes
    ++ allMimeAccept (Proxy :: Proxy ctypes)
    where ctypes = NE.toList $ contentTypes (Proxy :: Proxy ctype)

acceptCtype
  :: AllMimeAccept list a
  => Proxy list
  -> ByteString
  -> Maybe (SomeContentType a)
acceptCtype proxy = mapContentMedia (allMimeAccept proxy)


-- * Image helpers

toMediaType :: SourceFormat -> MediaType
toMediaType f = "image" // case f of
  SourceBitmap -> "bmp"
  SourceGif    -> "gif"
  SourceJpeg   -> "jpeg"
  SourcePng    -> "png"
  SourceTiff   -> "tiff"
  SourceHDR    -> "vnd.radiance"
  SourceTGA    -> "x-targa"


{-| Decodes the image with the format specified
-}
decodeImage' :: SourceFormat -> ByteString -> Either String DynamicImage
decodeImage' f bs = case f of
  SourceBitmap -> decodeBitmap bs
  SourceGif    -> decodeGif bs
  SourceJpeg   -> decodeJpeg bs
  SourcePng    -> decodePng bs
  SourceTiff   -> decodeTiff bs
  SourceHDR    -> decodeHDR bs
  SourceTGA    -> decodeTga bs

-- | Encodes a 'DynamicImage' into a lazy bytestring in a given content type
encodeImage :: SourceFormat -> DynamicImage -> Either String BL.ByteString
encodeImage ctype img = case ctype of
  SourceBitmap -> pure $ imageToBitmap img
  SourceGif    -> imageToGif img
  SourceJpeg   -> pure $ imageToJpg 80 img
  SourcePng    -> pure $ imageToPng img
  SourceTiff   -> pure $ imageToTiff img
  SourceHDR    -> pure $ imageToRadiance img
  SourceTGA    -> pure $ imageToTga img


-- | Similar to 'resizeAndCrop', but with Dynamic images
-- See 'rescaleDims' for the usage of the first argument
resizeAndCrop' :: (Maybe Int, Maybe Int) -> DynamicImage -> Maybe DynamicImage
resizeAndCrop' x img = case img of
  ImageY8     y -> Just $ ImageY8 $ resizeAndCrop x y
  ImageY16    y -> Just $ ImageY16 $ resizeAndCrop x y
  ImageY32    y -> Just $ ImageY32 $ resizeAndCrop x y
  ImageYF     _ -> Nothing
  ImageYA8    y -> Just $ ImageYA8 $ resizeAndCrop x y
  ImageYA16   y -> Just $ ImageYA16 $ resizeAndCrop x y
  ImageRGB8   y -> Just $ ImageRGB8 $ resizeAndCrop x y
  ImageRGB16  y -> Just $ ImageRGB16 $ resizeAndCrop x y
  ImageRGBF   _ -> Nothing
  ImageRGBA8  y -> Just $ ImageRGBA8 $ resizeAndCrop x y
  ImageRGBA16 y -> Just $ ImageRGBA16 $ resizeAndCrop x y
  ImageYCbCr8 y -> Just $ ImageYCbCr8 $ resizeAndCrop x y
  ImageCMYK8  y -> Just $ ImageCMYK8 $ resizeAndCrop x y
  ImageCMYK16 y -> Just $ ImageCMYK16 $ resizeAndCrop x y



-- | Resizes and crops an image such that the largest part remains and the image
-- has the right target size
resizeAndCrop
  :: (Pixel a, Bounded (PixelBaseComponent a), Integral (PixelBaseComponent a))
  => (Maybe Int, Maybe Int)
  -> Image a
  -> Image a
resizeAndCrop (  Nothing, Nothing) img = img
resizeAndCrop x@(Just _ , Nothing) img = resizeImg x img
resizeAndCrop x@(Nothing, Just _ ) img = resizeImg x img
resizeAndCrop x@(Just x0, Just x1) img =
  cropCentered (x0, x1) . resizeImg x $ img

-- | Applies the image resize for Juicy Pixel images
resizeImg
  :: (Pixel a, Bounded (PixelBaseComponent a), Integral (PixelBaseComponent a))
  => (Maybe Int, Maybe Int)
  -> Image a
  -> Image a
resizeImg x img =
  uncurry scaleBilinear (rescaleDims x (imageWidth img, imageHeight img)) img

-- | Applies the image cropping for Juicy Pixel images
cropCentered :: Pixel a => (Int, Int) -> Image a -> Image a
cropCentered (x0', x1') img = crop
  (round (fromIntegral (imageWidth img - x0) / 2 :: Double)) -- Start X index
  (round (fromIntegral (imageHeight img - x1) / 2 :: Double)) -- Start Y index
  x0 -- Target width
  x1 -- Target height
  img
 where
  x0 = min (imageWidth img) x0'
  x1 = min (imageHeight img) x1'

-- | Calculates the desired dimensions of a rescaled image
--  - If either one of the pairs is Nothing, keep aspect ratio is assumed.
--      The image will be rescaled such that its witdh / height matches what is specified
--  - If both are Nothing, the image dimensions are given
--  - If both are Just, the desired width and height is given such that the resulting
--     image, after cropping contains most of the original image
--
-- The image is never enlarged
rescaleDims :: (Maybe Int, Maybe Int) -> (Int, Int) -> (Int, Int)
rescaleDims (Nothing, Nothing) y          = y
rescaleDims (Just x , Nothing) y@(y0, y1) = if x > y0
  then y
  else (x, round (fromIntegral (x * y1) / fromIntegral y0 :: Double))
rescaleDims (Nothing, Just x) y@(y0, y1) = if x > y1
  then y
  else (round (fromIntegral (x * y0) / fromIntegral y1 :: Double), x)
rescaleDims (Just x0, Just x1) y = rescaleDims droppedMinimal y
 where
  toRatio :: (Int, Int) -> Double
  toRatio = uncurry (/) . fromIntegralR2
   where
    fromIntegralR2 :: (Integral a, Num b) => (a, a) -> (b, b)
    fromIntegralR2 (z0, z1) = (fromIntegral z0, fromIntegral z1)

  curRatio :: Double
  curRatio = toRatio y

  desRatio :: Double
  desRatio = toRatio (x0, x1)

  droppedMinimal :: (Maybe Int, Maybe Int)
  droppedMinimal =
    if desRatio > curRatio then (Just x0, Nothing) else (Nothing, Just x1)
