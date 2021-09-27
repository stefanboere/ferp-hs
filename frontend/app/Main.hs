{-# LANGUAGE CPP #-}
module Main
  ( main
  )
where

#if defined(ghcjs_HOST_OS)
import           Frontend                       ( main )
# else
import           Frontend                       ( mainWithHead )

main :: IO ()
main = mainWithHead
#endif
