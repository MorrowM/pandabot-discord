import           Pandabot.Modtools
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.State       as P
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toggleLockdownPure" $ do
    it "changes the lockdown state" $
      let allVals = [minBound..maxBound] :: [LockdownState]
      in and (zipWith (/=) allVals (toggleLockdownPure <$> allVals))

  describe "toggleLockdown" $
    it "uses toggleLockdownPure" $
      inplaceMatchesPure Locked <> inplaceMatchesPure Unlocked


inplaceMatchesPure :: LockdownState -> Expectation
inplaceMatchesPure st = P.run $ P.evalState st $ P.atomicStateToState @LockdownState $ do
  bef <- P.get @LockdownState
  aft <- toggleLockdown
  pure $ aft `shouldBe` toggleLockdownPure bef
