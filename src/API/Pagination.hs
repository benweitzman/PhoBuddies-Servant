{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module API.Pagination where 

import Data.ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Conversion

import Data.Maybe

import Data.Monoid

import Data.Proxy

import GHC.TypeLits

import Servant.API
import Servant.Utils.Links

data Pagination = Pagination 
    { prevPage :: Maybe URI
    , nextPage :: Maybe URI
    , firstPage :: URI
    , lastPage :: URI
    }

mkPagination :: (Integral a, HasLink end, MkLink end ~ (a -> URI), IsElem end api) 
    => a -> a -> Proxy end -> Proxy api -> Pagination
mkPagination 1 1 end api = Pagination Nothing Nothing (safeLink api end 1) (safeLink api end 1)
mkPagination 1 lastPage end api = Pagination Nothing (Just $ safeLink api end 2) (safeLink api end 1) (safeLink api end lastPage)
mkPagination curPage lastPage end api
    | curPage == lastPage = Pagination (Just $ safeLink api end (curPage - 1)) Nothing (safeLink api end 1) (safeLink api end lastPage)
    | otherwise = Pagination (Just $ safeLink api end (curPage - 1)) (Just $ safeLink api end (curPage + 1)) (safeLink api end 1) (safeLink api end lastPage)

type instance IsElem' (Get x t) (Get a (Headers hs t)) = ()

instance ToByteString Pagination where
    builder (Pagination prev next first last) = 
        byteString . intercalate ", " $ catMaybes
            [ uriToByteString "prev" <$> prev
            , uriToByteString "next" <$> next
            , uriToByteString "first" <$> Just first
            , uriToByteString "last" <$> Just last
            ]

uriToByteString :: ByteString -> URI -> ByteString
uriToByteString rel uri = "<" <> pack (show uri) <> ">; rel=\"" <> rel <> "\""