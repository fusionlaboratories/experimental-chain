use core::convert::From;
use serde::{Deserialize, Serialize};
use std::convert::TryInto;
use std::{mem::size_of, u128, u32};
use winter_math::fields::f64::BaseElement;

// TODO: pick a U256 from existing libs?
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
struct U256 {
    high: u128,
    low: u128,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Transcript {
    l1: u32,
    block_number: u64,
    block_hash: U256,
    transaction_index: u64,
    transaction_hash: U256,
    dest_address: U256,
    amount: u32,
}

const TRANSCRIPT_SIZE: usize = ( 4 * size_of::<u32>() + 3 * size_of::<U256>() ) / size_of::<u32>();

// TODO: I think there is an existing Rust trait that does it
pub fn to_elements(t: Transcript) -> [BaseElement; TRANSCRIPT_SIZE] {
    fn push32(bytes: &mut Vec<BaseElement>, u: u32) {
        bytes.push(From::from(u));
    }

    fn push64(bytes: &mut Vec<BaseElement>, u: u64) {
        bytes.push(From::from(u));
    }

    fn push128(bytes: &mut Vec<BaseElement>, u: u128) {
        for i in 0..4 {
            push32(bytes, (u >> ((3 - i) * 32)) as u32);
        }
    }

    fn push256(bytes: &mut Vec<BaseElement>, u: U256) {
        push128(bytes, u.high);
        push128(bytes, u.low);
    }

    let bytes = &mut Vec::<BaseElement>::with_capacity(TRANSCRIPT_SIZE);
    push32(bytes, t.l1);
    push64(bytes, t.block_number);
    push256(bytes, t.block_hash);
    push64(bytes, t.transaction_index);
    push256(bytes, t.transaction_hash);
    push256(bytes, t.dest_address);
    push32(bytes, t.amount);

    return bytes.to_vec().try_into().unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    const T1: Transcript = Transcript {
        l1: 1,
        block_number: 1,
        block_hash: U256 { high: 1, low: 0 },
        transaction_index: 1,
        transaction_hash: U256 { high: 0, low: 1 },
        dest_address: U256 { high: 1, low: 1 },
        amount: 1,
    };

    #[test]
    fn elements() {
        let bs = to_elements(T1);
        assert_eq!(TRANSCRIPT_SIZE, bs.len());
    }

    #[test]
    fn json() -> serde_json::Result<()> {
        let bs = serde_json::to_vec(&T1)?;
        let t = serde_json::from_slice(bs.as_slice())?;
        assert_eq!(T1, t);
        return Ok(());
    }
}
