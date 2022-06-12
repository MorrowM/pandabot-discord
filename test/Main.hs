module Main (main) where

import Pandabot.Modtools
import Polysemy qualified as P
import Polysemy.AtomicState qualified as P
import Polysemy.State qualified as P
import Test.Sandwich

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec

spec :: TopSpec
spec = do
  describe "toggleLockdownPure" do
    it "changes the lockdown state"
      let allVals = [minBound..maxBound] :: [LockdownState]
      in shouldBe True $ and (zipWith (/=) allVals (toggleLockdownPure <$> allVals))

  describe "toggleLockdown" do
    it "uses toggleLockdownPure" do
      inplaceMatchesPure Locked
      inplaceMatchesPure Unlocked

  where
    inplaceMatchesPure st = P.run $ P.evalState st $ P.atomicStateToState @LockdownState do
      bef <- P.get @LockdownState
      aft <- toggleLockdown
      pure $ aft `shouldBe` toggleLockdownPure bef
