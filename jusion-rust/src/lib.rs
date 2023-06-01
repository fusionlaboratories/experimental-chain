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

pub fn load_verifier_files<P: AsRef<Path>> (hash_path: &P, outputs_path: &P, proof_path: &P) -> (ProgramInfo, StackOutputs, ExecutionProof) {
    let hash_bytes = fs::read(hash_path).unwrap();
    let outputs_bytes = fs::read(outputs_path).unwrap();
    let proof_bytes = fs::read(proof_path).unwrap();

    let hash = Digest::read_from_bytes(&hash_bytes).unwrap();
    let out_file: OutputFile = serde_json::from_slice(&outputs_bytes).unwrap();
    let outputs = out_file.stack_outputs();
    let proof = ExecutionProof::from_bytes(&proof_bytes).unwrap();

    let info = ProgramInfo::new(hash, Kernel::default());
    return (info, outputs, proof);
}

#[cfg(test)]
mod tests {
    use miden_core::FieldElement;
    use tempfile::tempdir;

    use super::*;
    use std::path::PathBuf;

    const T1: Transcript = Transcript {
        l1: 1,
        block_number: 1,
        block_hash: U256 { high: 1, low: 0 },
        transaction_index: 1,
        transaction_hash: U256 { high: 0, low: 1 },
        dest_address: U256 { high: 1, low: 1 },
        amount: 1,
    };

    fn setup_verifier_test() -> (Transcript, (ProgramInfo, StackOutputs, ExecutionProof)) {
        let dir = tempdir().unwrap();

        let hash_path    = dir.path().join("hash");
        let outputs_path = dir.path().join("outputs");
        let proof_path   = dir.path().join("proof");
        println!("{:?}", dir);

        let input = PathBuf::new().join("../jusion-haskell/tests/test.masm");
        run_prover_files(T1, &input, &hash_path, &outputs_path, &proof_path);
        return (T1, load_verifier_files(&hash_path, &outputs_path, &proof_path));
    }

    #[test]
    fn roundtrip() -> () {
        let (t, (info, outputs, proof)) = setup_verifier_test();
        assert!(run_verifier(t, info, outputs, proof).is_ok());
    }

    #[test]
    fn tamper_transcript() -> () {
        todo!();
    }

    #[test]
    fn tamper_hash() -> () {
        let (t, (_, outputs, proof)) = setup_verifier_test();;
        let one = BaseElement::ONE;
        let d = Digest::new([one, one, one, one]);
        let i = ProgramInfo::new(d, Kernel::default());
        assert!(run_verifier(t, i, outputs, proof).is_err());
    }

    #[test]
    fn tamper_outputs() -> () {
        let (t, (info, mut outputs, proof)) = setup_verifier_test();
        outputs.stack_mut()[0] += 1;
        assert!(run_verifier(t, info, outputs, proof).is_err());
    }

    #[test]
    fn tamper_proof() -> () {
        let (t, (info, outputs, proof)) = setup_verifier_test();
        let mut bs = proof.to_bytes();
        bs[1] += 1;
        let p = ExecutionProof::from_bytes(&bs).unwrap();
        assert!(run_verifier(t, info, outputs, p).is_err());
    }

    #[test]
    fn json() -> serde_json::Result<()> {
        let bs = serde_json::to_vec(&T1)?;
        let t = serde_json::from_slice(&bs)?;
        assert_eq!(T1, t);
        return Ok(());
    }
}
