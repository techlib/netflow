{-|
Module      :  Data.MAC
Description :  MAC address handling
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Data.MAC
where
  import BasePrelude


  newtype MAC = MAC
    { macOctets :: (Word8, Word8, Word8, Word8, Word8, Word8) }
    deriving (Eq, Ord, Typeable)

  instance Show MAC where
    show mac = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f
      where (a, b, c, d, e, f) = macOctets mac


-- vim:set ft=haskell sw=2 ts=2 et:
