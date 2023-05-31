use core::convert::From;
use miden_assembly::{Assembler, AssemblyError};
use miden_core::{Kernel, Program, ProgramInfo, StackInputs, StackOutputs };
use miden_prover::{Digest, ExecutionProof, MemAdviceProvider, prove};
use miden_verifier::{VerificationError, verify};
use serde::{Deserialize, Serialize};
use std::{convert::TryInto, fs, mem::size_of, path::Path, u128, u32};
use winter_math::fields::f64::BaseElement;
use winter_utils::{Deserializable, Serializable};

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

// Inlined from miden as it's not exported
// https://github.com/0xPolygonMiden/miden-vm/blob/4195475d75ab2d586bdb01d1ff3ea2cd626eaf7b/miden/src/cli/data.rs#LL260C1-L264C2
#[derive(Deserialize, Serialize, Debug)]
pub struct OutputFile {
    pub stack: Vec<String>,
    pub overflow_addrs: Vec<String>,
}

impl OutputFile {
    /// Returns a new [OutputFile] from the specified outputs vectors
    pub fn new(stack_outputs: &StackOutputs) -> Self {
        Self {
            stack: stack_outputs.stack().iter().map(|&v| v.to_string()).collect::<Vec<String>>(),
            overflow_addrs: stack_outputs
                .overflow_addrs()
                .iter()
                .map(|&v| v.to_string())
                .collect::<Vec<String>>(),
        }
    }

    /// Converts outputs vectors for stack and overflow addresses to [StackOutputs].
    pub fn stack_outputs(&self) -> StackOutputs {
        let stack = self.stack.iter().map(|v| v.parse::<u64>().unwrap()).collect::<Vec<u64>>();

        let overflow_addrs = self
            .overflow_addrs
            .iter()
            .map(|v| v.parse::<u64>().unwrap())
            .collect::<Vec<u64>>();

        StackOutputs::new(stack, overflow_addrs)
    }
}

pub fn transcript(t: Transcript) -> StackInputs {
    return StackInputs::new(to_elements(t).to_vec());
}

pub fn read_program<P: AsRef<Path>> (path: P) -> Result<Program, AssemblyError> {
    let data = fs::read_to_string(path).expect("Unable to read file");
    let assembler = Assembler::default();
    return assembler.compile(data);
}

pub fn run_prover (t: Transcript, program: Program) -> (Digest, StackOutputs, ExecutionProof) {
    let (outputs, proof) = prove(&program, transcript(t), MemAdviceProvider::default(), Default::default()).unwrap();
    return (program.hash(), outputs, proof);
}

pub fn run_verifier(t: Transcript, program_info: ProgramInfo, outputs: StackOutputs, proof: ExecutionProof) -> Result<u32, VerificationError> {
    return verify(program_info, transcript(t), outputs, proof);
}

pub fn run_prover_files<P: AsRef<Path>> (t: Transcript, program_path: &P, hash_path: &P, outputs_path: &P, proof_path: &P) {
    let program = read_program(program_path).unwrap();
    let (hash, outputs, proof) = run_prover(t, program);

    fs::write(hash_path, hash.to_bytes()).unwrap();
    let outputs_bytes = serde_json::to_vec_pretty(&OutputFile::new(&outputs)).unwrap();
    fs::write(outputs_path, outputs_bytes).unwrap();
    fs::write(proof_path, proof.to_bytes()).unwrap();
}

pub fn run_verifier_files<P: AsRef<Path>> (t: Transcript, hash_path: &P, outputs_path: &P, proof_path: &P) -> Result<u32, VerificationError> {
    let hash_bytes = fs::read(hash_path).unwrap();
    let outputs_bytes = fs::read(outputs_path).unwrap();
    let proof_bytes = fs::read(proof_path).unwrap();

    let hash = Digest::read_from_bytes(&hash_bytes).unwrap();
    let out_file: OutputFile = serde_json::from_slice(&outputs_bytes).unwrap();
    let outputs = out_file.stack_outputs();
    let proof = ExecutionProof::from_bytes(&proof_bytes).unwrap();

    let info = ProgramInfo::new(hash, Kernel::default());
    return run_verifier(t, info, outputs, proof);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};

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
    fn roundtrip() {
        let tmp = std::env::temp_dir();
        let hash_path    = tmp.join("hash");
        let outputs_path = tmp.join("outputs");
        let proof_path   = tmp.join("proof");
        println!("{:?}", tmp);

        let input = PathBuf::new().join("../jusion-haskell/tests/test.masm");
        run_prover_files(T1, &input, &hash_path, &outputs_path, &proof_path);
        run_verifier_files(T1, &hash_path, &outputs_path, &proof_path).unwrap();
    }

    #[test]
    fn json() -> serde_json::Result<()> {
        let bs = serde_json::to_vec(&T1)?;
        let t = serde_json::from_slice(&bs)?;
        assert_eq!(T1, t);
        return Ok(());
    }
}
