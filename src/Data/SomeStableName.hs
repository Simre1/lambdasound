module Data.SomeStableName where

import Data.Hashable
import GHC.StableName
import Control.Monad.IO.Class

data SomeStableName = forall a. SomeStableName (StableName a)

instance Eq SomeStableName where
  (SomeStableName sn1) == (SomeStableName sn2) = sn1 `eqStableName` sn2

instance Hashable SomeStableName where
  hashWithSalt salt (SomeStableName sn) = salt * hashStableName sn
  hash (SomeStableName sn) = hashStableName sn

makeSomeStableName :: MonadIO m => a -> m SomeStableName
makeSomeStableName = liftIO . fmap SomeStableName . makeStableName
