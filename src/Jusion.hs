module Jusion where

-- Here we assume that there are only two L1 chains available
--   - Some other L1 chain, and
--   - Jusion L1 chain.
--
-- Example program in Kachina style
--
--   let b : Block = query L1.getBlockByHeight {height}
--   assert b.hash == {hash}
--
-- Actually, the above program will get simplified to a single public oracle
-- query, or rather maybe this should be called a _fact_?
--
--   L1.Block {height} {hash}

-- start
--   let b = query L1.getBlockByHeight {height}
--   assert b.hash == {hash}

-- desugar data structs
--   let (L1.Block b_height b_hash) = L1.getBlockByHeight {height}
--   assert b_hash == {hash}

-- desugar queries
--   let (L1.Block b_height b_hash) = fact L1.Block {height} {?}
--   assert b_hash == {hash}

-- pull fact out of let
--   let b_height = {height}
--   let b_hash = {?}
--   fact L1.Block b_height b_hash
--   assert b_hash == {hash}

-- unify b_hash and {hash}
--   let b_height = {height}
--   let b_hash = {hash}
--   fact L1.Block b_height b_hash