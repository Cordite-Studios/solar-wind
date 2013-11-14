
import Solar.Cast.Time
import Solar.Cast.Internal.Time

mkDemoTicker time i' = do
    let i = fromInteger i'
        t = Tick 0 time i
    tv <- newTVar t
    return $ DeltaContainer (Ticker tv i) i
mkDemo = do
    time <- getCurrentTime
    atomically $ do
        ts <- mapM (mkDemoTicker time) [1..10]
        qts <- newTVar $ foldr insert [] ts
        tsv <- newTVar $ TickingClock qts time Nothing
        return tsv

showDemo tc' = do
    putStrLn "Current List:"
    ts <- atomically $ do
        tc <- readTVar tc'
        t <- readTVar $ deltaTicker tc
        forM t $ \tk -> do
            v <- readTVar $ tick.content $ tk
            return (v, remaining tk)
    forM_ ts $ \t -> do
        putStrLn $ show t

doDemo x = do
    d <- mkDemo
    showDemo d
    time <- getCurrentTime
    let endtime = addUTCTime (fromInteger x) time
    stepUntil d time 1.0 endtime
doDemo' x = do
    d <- mkDemo
    showDemo d
    replicateM_ x $ do
        threadDelay 1000000
        incClock d
        showDemo d


stepUntil d time diff endtime = do
    atomically $ incClock' ntime diff d
    showDemo d
    when (ntime < endtime) $ stepUntil d ntime diff endtime
    where ntime = addUTCTime diff time