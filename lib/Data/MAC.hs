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

  import Data.Aeson


  newtype MAC = MAC
    { macOctets :: (Word8, Word8, Word8, Word8, Word8, Word8) }
    deriving (Eq, Ord, Typeable)

  instance Show MAC where
    show mac = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f
      where (a, b, c, d, e, f) = macOctets mac


  toMAC :: [Word8] -> MAC
  toMAC [a, b, c, d, e, f] = MAC (a, b, c, d, e, f)
  toMAC _ = error "invalid number of MAC octets"


  instance ToJSON MAC where
    toJSON = toJSON . show


-- vim:set ft=haskell sw=2 ts=2 et:
