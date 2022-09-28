module BracketSpec where

import Test.Hspec

import Data.IORef
import Control.Exception

data Action
  = Outer
  | Middle
  | Inner
  deriving (Show, Eq)

writeLog :: IORef [String] -> String -> IO ()
writeLog ioRef msg = modifyIORef' ioRef (msg :)

action msgVar act True _ = do
  let msg = show act <> " failed"
  writeLog msgVar msg
  error msg
action msgVar act False res = writeLog msgVar $ show act <> ": " <> show res

create msgVar act True = do
  let msg = show act <> " create failed"
  writeLog msgVar msg
  error msg
create msgVar act False = do
  let msg = show act <> " created"
  writeLog msgVar msg
  pure (act, "some resource")

dispose msgVar act True _ = do
  let msg = show act <> " dispose failed"
  writeLog msgVar msg
  error msg
dispose msgVar act False res = do
  let msg = show act <> " disposed: " <> show res
  writeLog msgVar msg


spec :: Spec
spec =
  describe "bracket tests" $ do
    it "Inner action throws error" $ do

      msgVar <- newIORef []

      eRes <- try $
        bracket (create msgVar Outer  False) (dispose msgVar Outer  False) $ \outerRes  ->
        bracket (create msgVar Middle False) (dispose msgVar Middle False) $ \middleRes ->
        bracket (create msgVar Inner  False) (dispose msgVar Inner  False) $ \innerRes  ->
          action msgVar Inner True (outerRes, middleRes, innerRes)

      case eRes of
        Left (_ :: SomeException) -> do
          msgs <- readIORef msgVar
          reverse msgs `shouldBe`
            [ "Outer created"
            , "Middle created"
            , "Inner created"
            , "Inner failed"
            , "Inner disposed: (Inner,\"some resource\")"
            , "Middle disposed: (Middle,\"some resource\")"
            , "Outer disposed: (Outer,\"some resource\")"
            ]
        Right _ -> fail "Unexpected success"

    it "Inner create throws error" $ do

      msgVar <- newIORef []

      eRes <- try $
        bracket (create msgVar Outer  False) (dispose msgVar Outer  False) $ \outerRes  ->
        bracket (create msgVar Middle False) (dispose msgVar Middle False) $ \middleRes ->
        bracket (create msgVar Inner  True)  (dispose msgVar Inner  False) $ \innerRes  ->
          action msgVar Inner False (outerRes, middleRes, innerRes)

      case eRes of
        Left (_ :: SomeException) -> do
          msgs <- readIORef msgVar
          reverse msgs `shouldBe`
            [ "Outer created"
            , "Middle created"
            , "Inner create failed"
            , "Middle disposed: (Middle,\"some resource\")"
            , "Outer disposed: (Outer,\"some resource\")"
            ]
        Right _ -> fail "Unexpected success"

    it "Inner dispose throws error" $ do

      msgVar <- newIORef []

      eRes <- try $
        bracket (create msgVar Outer  False) (dispose msgVar Outer  False) $ \outerRes  ->
        bracket (create msgVar Middle False) (dispose msgVar Middle False) $ \middleRes ->
        bracket (create msgVar Inner  False) (dispose msgVar Inner  True)  $ \innerRes  ->
          action msgVar Inner False (outerRes, middleRes, innerRes)

      case eRes of
        Left (_ :: SomeException) -> do
          msgs <- readIORef msgVar
          reverse msgs `shouldBe`
            [ "Outer created"
            , "Middle created"
            , "Inner created"
            , "Inner: ((Outer,\"some resource\"),(Middle,\"some resource\"),(Inner,\"some resource\"))"
            , "Inner dispose failed"
            , "Middle disposed: (Middle,\"some resource\")"
            , "Outer disposed: (Outer,\"some resource\")"
            ]
        Right _ -> fail "Unexpected success"



