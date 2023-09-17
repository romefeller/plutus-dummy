{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Ledger
import Ledger.Tx as Ada
import Ledger.Tx.Constraints as Constraints
import Plutus.Trace.Emulator as Emulator
import Plutus.Contract
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified Prelude as P
import Data.Text
import Wallet.Emulator
import Control.Monad (void, forever)
import Crypto.Hash
import Data.ByteArray.Encoding (convertFromBase, Base(Base16))
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Char (ord)

charToWord8 :: P.Char -> Word8
charToWord8 = P.fromIntegral . ord

mockHash :: Digest Blake2b_160
mockHash = fromMaybe (P.error "error")
           (digestFromByteString (BS.pack (P.map charToWord8 "0x0000000000000000000")))

-- Basic validation function
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

data MyRedeemer
-- instance FromData MyRedeemer where
   -- fromData = error ()
-- instance ToData MyRedeemer where
   -- toData = error ()

-- Compile to Plutus script
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = validatorHash (Versioned validator PlutusV2)

-- Wallets and simulation
hello ::  Contract () DummySchema Text ()
hello = logInfo @P.String "Hello, world!"

type DummySchema = Endpoint "dummy" ()

-- Example simulation
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet Nothing (WalletId mockHash)) hello
    callEndpoint @"dummy" h ()
    void P.$ Emulator.waitNSlots 1

-- Main function
main :: P.IO ()
main = runEmulatorTraceIO myTrace

