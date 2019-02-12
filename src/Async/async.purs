module Async(
  Async,
  runAsync,
  copyFileAsync
) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Effect (Effect)
import Effect.Class.Console (log)

-- foreign import data Async :: Type -> Type
-- foreign import fromCb :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Async a
-- foreign import runAsync :: forall a. Async a -> Effect Unit

-- -- doGet url = fromCb :: ajaxGet url
-- -- ajaxGet :: String -> (Resp -> Effect Unit) -> Effect Unit


-- x :: Async String
-- x = fromCb (\_ -> log "hi")

type ErrorCode = String
type FilePath = String

foreign import readFileImpl
  :: Fn3 FilePath
         (String -> Effect Unit)
         (ErrorCode -> Effect Unit)
         (Effect Unit)

foreign import writeFileImpl
  :: Fn4 FilePath
         String
         (Effect Unit)
         (ErrorCode -> Effect Unit)
         (Effect Unit)

-- newtype ContT r m a = ContT ((a -> m r) -> m r)
-- type Async2 = ContT Unit Effect

newtype Async a = Async ((a -> Effect Unit) -> Effect Unit)

readFile :: FilePath -> (Either ErrorCode String -> Effect Unit) -> Effect Unit
readFile p fe = runFn3 readFileImpl p (fe <<< Right) (fe <<< Left)

writeFile :: FilePath -> String -> (Either ErrorCode Unit -> Effect Unit) -> Effect Unit
writeFile p d fe = runFn4 writeFileImpl p d (fe $ Right unit) (fe <<< Left)

readFileAsync :: FilePath -> Async (Either ErrorCode String)
readFileAsync p = Async $ readFile p

writeFileAsync :: FilePath -> String -> Async (Either ErrorCode Unit)
writeFileAsync p d = Async $ writeFile p d

-- copyFileAsync :: FilePath -> FilePath -> Async (Either ErrorCode Unit)
-- copyFileAsync src dest = do
--   e <- readFileAsync src
--   case e of
--     Left err -> pure $ Left err
--     Right content -> writeFileAsync dest content

pureAsync :: forall a. a -> Async a
pureAsync a = Async \k -> k a

bindAsync :: forall a b. Async a -> (a -> Async b) -> Async b
bindAsync (Async fa) f = Async (\b -> fa (\a -> case f a of Async a' -> a' b))

copyFileAsync :: FilePath -> FilePath -> Async (Either ErrorCode Unit)
copyFileAsync src dest = readFileAsync src `bindAsync` \e ->
  case e of
    Left err -> pureAsync $ Left err
    Right content -> writeFileAsync dest content
-- do
--   e <- readFileAsync src
--   case e of
--     Left err -> pure $ Left err
--     Right content -> writeFileAsync dest content

runAsync :: forall a. Async a -> Effect Unit
runAsync (Async f) = f \a -> log ""
