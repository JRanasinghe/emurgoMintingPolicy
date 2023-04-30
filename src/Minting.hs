{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}

module Minting where

-- import           Data.Aeson          (ToJSON, FromJSON)
-- import           GHC.Generics (Generic)
import qualified PlutusTx
import qualified PlutusTx.AssocMap         as M
import           PlutusTx.Prelude
import           Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)  -- (mkUntypedValidator)
-- import qualified Ledger                    as L
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts as V2LC
-- import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2V

-- data MintAction = Mint | Burn
--   deriving (Generic) --, FromJSON, ToJSON)
-- PlutusTx.unstableMakeIsData ''MintAction

-- | A workaround since I could not make 'data MintAction = Mint | Burn' work
type MintAction = Bool

treasuryValHash :: ValidatorHash
treasuryValHash = ValidatorHash "deadbeef"

mintingCost :: Integer
mintingCost = 75_000_000

symbolValueOf' :: CurrencySymbol -> Value -> (Integer, Integer)
symbolValueOf' cs v = case M.lookup cs (getValue v) of
  Nothing       -> (0, 0)
  Just csTokens -> (minted, burned)
    where
      minted = foldr sum' 0 . M.toList $ M.filter (>0) csTokens
      burned = foldr sum' 0 . M.toList $ M.filter (<0) csTokens
      sum' = \(_, a) b -> a + b

{-# INLINABLE emurgoMintingPolicyT #-}
-- | Typed minting policy
emurgoMintingPolicyT :: TxOutRef -> MintAction -> V2LC.ScriptContext -> Bool
emurgoMintingPolicyT oref redeemer ctx =
  let
    info             = scriptContextTxInfo ctx
    (minted, burned) = symbolValueOf' (ownCurrencySymbol ctx) (txInfoMint info)
  in
    case redeemer of
      -- 'False' stands for 'Mint'
      False ->   minted == 1
              && burned == 0
              && (any (\txo -> case addressCredential . txOutAddress $ txo of
                          PubKeyCredential _  -> False
                          ScriptCredential vh -> vh == treasuryValHash &&
                            valueOf (txOutValue txo) adaSymbol adaToken >= mintingCost) $
                      txInfoOutputs info)
              && (any (\txIn -> txInInfoOutRef txIn == oref) $ txInfoInputs info)
      -- 'True' stands for 'Burn'
      True  -> minted == 0 && burned < 0

-- | Minting policy compiled to Plutus Core
policy :: TxOutRef -> MintingPolicy
policy oref = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| mkUntypedMintingPolicy . emurgoMintingPolicyT ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode oref
