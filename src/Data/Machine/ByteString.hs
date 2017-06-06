{-# LANGUAGE Rank2Types #-}
module Data.Machine.ByteString (
  -- * Sources
  fromLazy,
  fromStrict,
  fromHandle,
  hGet,
  hGetSome,
  hGetNonBlocking,
  -- * Sinks
  stdin,
  stdout,
  stderr,
  toHandle,
  interact,
  -- * Processes
  mapping,
  filtered,
  taking,
  takingWhile,
  scan,
  -- Stream Transformation
  pack,
  unpack
  ) where

import           Data.Word (Word8)
import           Data.Machine hiding (filtered, mapping, taking, takingWhile, scan)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Unsafe (unsafeTake)
import           Data.ByteString.Lazy.Builder.Extras (defaultChunkSize)
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as IO
import           Control.Monad.IO.Class (MonadIO(..))
import           Prelude hiding (interact)

-- | Transform a lazy ByteString into a 'Source' of strict 'ByteString'.
fromLazy :: BL.ByteString -> Source ByteString
fromLazy bs = construct $ BL.foldrChunks (\e a -> yield e >> a) (pure ()) bs

-- | Use a strict ByteString as Source
fromStrict :: ByteString -> Source ByteString
fromStrict bs = construct $ yield bs

-- | Use an 'Handle' to create a 'Source' of strict 'ByteString'.
fromHandle :: MonadIO m => IO.Handle -> SourceT m ByteString
fromHandle = hGetSome defaultChunkSize

-- | Read chunks of strict 'ByteString' of fixed size.
hGet :: MonadIO m => Int -> IO.Handle -> SourceT m ByteString
hGet size h = repeatedly $ do
  bs <- liftIO (B.hGet h size)
  if B.null bs
     then pure ()
     else yield bs

-- | Like 'hGet' but may be returned smaller chunks if there are not enough
-- data immediately available.
hGetSome :: MonadIO m => Int -> IO.Handle -> SourceT m ByteString
hGetSome size h = repeatedly $ do
  bs <- liftIO (B.hGetSome h size)
  if B.null bs
     then pure ()
     else yield bs

-- | Similar to 'hGet' but it will never block waiting for data.
hGetNonBlocking :: MonadIO m => Int -> IO.Handle -> SourceT m ByteString
hGetNonBlocking size h = repeatedly $ do
  eof <- liftIO (IO.hIsEOF h)
  if eof
     then pure ()
     else liftIO (B.hGetNonBlocking h size)>>= yield

-- | Source that stream bytes from 'stdin'
stdin :: MonadIO m => SourceT m ByteString
stdin = fromHandle IO.stdin

-- | Create a 'Machine' that stream its input to the given 'Handle'
toHandle :: MonadIO m => IO.Handle -> MachineT m (Is ByteString) ()
toHandle h = autoM (liftIO . B.hPut h)

-- | Stream data to 'stdout'
stdout :: MonadIO m => MachineT m (Is ByteString) ()
stdout = toHandle IO.stdout

-- | Stream data to 'stderr'
stderr :: MonadIO m => MachineT m (Is ByteString) ()
stderr = toHandle IO.stderr

-- | Use a @Process ByteString ByteString@ to transform the input from
-- 'stdin' and output to 'stdout'
interact :: Process ByteString ByteString -> IO ()
interact p = runT_ $ stdin ~> p ~> stdout

-- | Transform every byte of a 'ByteString'
mapping :: (Word8 -> Word8) -> Process ByteString ByteString
mapping f = auto $ B.map f

-- | Allow to pass only the bytes that satisfy the predicate
filtered :: (Word8 -> Bool) -> Process ByteString ByteString
filtered p = auto $ B.filter p

-- | Stream only the first n bytes
taking :: Integral n => n -> Process ByteString ByteString
taking n0 = construct $ go n0
  where
    go n
        | n <= 0 = pure ()
        | otherwise = do
          bs <- await
          let len = fromIntegral (B.length bs)
          if len > n
             then yield (unsafeTake (fromIntegral n) bs)
             else yield bs >> go (n - len)

-- | Stream bytes until they fail the predicate
takingWhile :: (Word8 -> Bool) -> Process ByteString ByteString
takingWhile p = construct go
  where
    go = do
      bs <- await
      let (prefix, suffix) = B.span p bs
      if B.null suffix
         then yield bs >> go
         else yield prefix

-- Drop the first n bytes
-- dropping :: Integral n => n -> Process ByteString ByteString
-- dropping n = loop n
--   where
--     loop cnt
--       | cnt <= 0  = echo
--       | otherwise = do
--                       bs <- await
--                       let l = B.length bs
--                       if l <= cnt
--                          then loop (cnt - l)
--                          else pure . B.drop l >>= yield >> loop (cnt - l)


-- | Strict left scan over the bytes
scan :: (Word8 -> Word8 -> Word8) -> Word8 -> Process ByteString ByteString
scan step begin = construct $ do
    yield (B.singleton begin)
    go begin
  where
    go w8 = do
        bs <- await
        let bs' = B.scanl step w8 bs
            w8' = B.last bs'
        yield (B.tail bs')
        go w8'

-- | Convert a stream of strict 'ByteString' into a stream of Bytes
unpack :: Process ByteString Word8
unpack = auto B.unpack ~> asParts

-- | Convert a stream of Bytes into a stream of strict 'ByteString' of
-- size equal to 'defaultChunkSize'
pack :: Process Word8 ByteString
pack = buffered defaultChunkSize ~> auto B.pack
