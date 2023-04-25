module Main (main) where

import Lang (main)

-- A Simple Example of L1 Oracle

-- L1 Oracle



data BlockHeight
data BlockHash
data TransactionPosition
data TransactionHash
data Address
data Amount

data Transcipt
    -- L1 Oracle Operations
    = GetBlock BlockHeight BlockHash
    | GetTransaction BlockHeight BlockHash TransactionPosition TransactionHash
    -- Local state
    | RecordTransaction BlockHeight BlockHash TransactionPosition TransactionHash
    -- Account Operations
    | Widthdraw Address Amount
    | Deposit Address Amount
    -- Coin Operation
    | Mint Amount

-- Example observation of an L1 transaction
observe bHeight bHash txPos txHash address amount =
    [ GetBlock bHeight bHash
    , GetTransaction bHeight bHash txPos txHash
    , RecordTransaction bHeight bHash txPos txHash
    , Mint amount
    , Deposit address amount
    ]
{-
We will get an array of 18 field elements.

let input = make_array(18) in

      GetBlock bHeight bHash
    , GetTransaction bHeight bHash txPos txHash
    , RecordTransaction bHeight bHash txPos txHash
    , Mint amount
    , Deposit address amount
    
    input[1] == input[4]
-}

{-
    , RequestWidthdrawal bHeight bHash txPos txHash
    , RequestDeposit address amount

--
   , RecordWidthrawal bHeight bHash txPos txHash
   , Mint amount
   , Deposit address amount

-}
