{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO as S
import           Plutus.V2.Ledger.Api

import Utils
import Minting
import Deploy


main :: IO ()
main = do
  oref1Ascii <- S.readFile "./app/oref1_Id.tmp"
  let oref1 = TxOutRef
        { txOutRefId  = TxId . hexConvert $ oref1Ascii
        , txOutRefIdx = 0
        }
      policy_1 = policy oref1
  putStrLn "Writing minting policy..."
  writePolicy "compiled/emurgoMintingPolicy.plutus" policy_1
  putStrLn "Done"
