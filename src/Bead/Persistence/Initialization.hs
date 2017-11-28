{-# LANGUAGE Rank2Types #-}
module Bead.Persistence.Initialization where

-- | Persistence initializer is a collection of functions that checks if the persistence
-- is set up correctly, inicializes the persistent layer when it is not set up,
-- and tears down the database
data PersistInit = PersistInit {
    isSetUp     :: IO Bool
  , initPersist :: IO ()
  , tearDown    :: IO ()
  }
