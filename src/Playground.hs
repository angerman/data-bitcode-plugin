{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Playground where
import Control.Applicative (liftA2)
import Control.Monad (ap, liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Fix

data CtxA = CtxA { n :: Int } deriving Show
data CtxB = CtxB { m :: [Int] } deriving Show

newtype MaT m a = MaT { runMaT :: StateT CtxA m a } deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadIO)

modify_ :: (Monad m) => (CtxA -> CtxA) -> MaT m ()
modify_ f = MaT $ modify f

set :: (Monad m) => Int -> MaT m ()
set n = modify_ (\ctx -> ctx { n = n })

t :: CtxA -> CtxA
t (CtxA{..}) = let n' = n in let n = n' + 1 in CtxA{..}