{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Region where
import Control.Exception

-- | Represents an arbitrary handle to some resource.  The constructor and
--   accessor functions should not be exposed to the users of the library as
--   they are reserved for the implementors.
newtype Handle s a = Handle { fromHandle :: a }

-- | Ensure that the handle to a resource is released automatically when the
--   computation ends.
withHandle :: a                               -- ^ handle to the resource
           -> (a -> IO ())                    -- ^ for releasing the resource
           -> (forall s . Handle s a -> IO b) -- ^ computation to be executed
           -> IO b
withHandle grab drop f = bracket (return grab) drop (\ r -> f (Handle r))
