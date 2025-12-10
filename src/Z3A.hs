module Z3A (
    module Z3.Monad,
    runZ3,
    optimizeRun,
) where

import Control.Monad.IO.Class (liftIO)

import System.IO.Unsafe (unsafePerformIO)

import Z3.Monad

runZ3 :: Z3 a -> a
runZ3 = unsafePerformIO . evalZ3

optimizeRun :: Z3 Model
optimizeRun = do
    optimizeCheck [] >>= \case
        Sat -> pure ()
        r -> do
            liftIO . putStrLn =<< optimizeToString
            error $ show r
    optimizeGetModel
