#![allow(warnings)]

extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
use std::marker::PhantomData;
use serde::de::DeserializeOwned;
use rsa::{PublicKey, RsaPrivateKey, RsaPublicKey, PaddingScheme};
use rand::rngs::OsRng;
use rand::rngs::StdRng;
use rand::RngCore;
use rand::SeedableRng;
use openssl::bn::BigNum;
use openssl::pkey::{Public, Private};
use openssl::rsa::{Rsa, Padding};
use openssl::symm::{Cipher, encrypt, decrypt};
use openssl::hash::{MessageDigest};
use openssl::hash::hash as openhash;

#[derive(Serialize, Deserialize)]
pub struct Repr<T>(Vec<u8>, PhantomData<T>);

impl<T : Serialize + DeserializeOwned> Represent<T> for Repr<T> {
    fn from_repr(b: Repr<T>) -> T { bincode::deserialize(&b.0[..]).unwrap() }
    fn to_repr(b: T) -> Repr<T> { Repr(bincode::serialize(&b).unwrap(), PhantomData) }
}

trait Represent<T> {
    fn from_repr(_: Repr<T>) -> T;
    fn to_repr(_: T) -> Repr<T>;
}

fn send<E, P, A: marker::Send + Serialize + DeserializeOwned + 'static>(c: Chan<E, Send<Repr<A>, P>>, v: A) -> Chan<E, P> { c.send(Repr::to_repr(v)) }
fn recv<E, P, A: marker::Send + Serialize + DeserializeOwned + 'static>(c: Chan<E, Recv<Repr<A>, P>>) -> (Chan<E, P>, A) { let (c, x) = c.recv(); (c, Repr::from_repr(x)) }
fn close<E>(c: Chan<E, Eps>) { c.close() }

type AliceParent = Send<Repr<bytes>, Eps>;
type AliceTPM = Recv<Repr<bytes>, Send<Repr<bytes>, Recv<Repr<bytes>, Send<Repr<bytes>, Send<Repr<bytes>, Recv<Repr<bytes>, Eps>>>>>>;

type ParentTPM = Send<Repr<bytes>, Recv<Repr<bytes>, Send<Repr<bytes>, Choose<Recv<Repr<bytes>, Eps>, Recv<Repr<bytes>, Eps>>>>>;
type ParentAlice = Recv<Repr<bytes>, Eps>;

type TPMParent = Recv<Repr<bytes>, Send<Repr<bytes>, Recv<Repr<bytes>, Offer<Send<Repr<bytes>, Eps>, Send<Repr<bytes>, Eps>>>>>;
type TPMAlice = Send<Repr<bytes>, Recv<Repr<bytes>, Send<Repr<bytes>, Recv<Repr<bytes>, Recv<Repr<bytes>, Send<Repr<bytes>, Eps>>>>>>;

type pkey = Rsa<Public>;
type skey = Rsa<Private>;
type symkey = [u8; 16];
type bytes = Vec<u8>;
type stpkey = RsaPublicKey;
type stskey = RsaPrivateKey;

fn pk(a1: &skey) -> pkey {
	// Seemingly the only way to specifically generate a public key
	// is to use the n and e
	let n: BigNum = a1.n() + &(BigNum::new().unwrap());
	let e: BigNum = a1.e() + &(BigNum::new().unwrap());

	return Rsa::from_public_components(n, e).unwrap()
}
fn stpk(a1: stskey) -> stpkey {
	return RsaPublicKey::from(a1)
}
fn sign(a1: bytes, a2: &skey) -> bytes {
	let mut buf = vec![0; a2.size() as usize];
	a2.private_encrypt(&a1, &mut buf, Padding::PKCS1).unwrap();
	return buf
}
fn checksign(a1: bytes, a2: &pkey) -> bytes {
	let mut buf = vec![0; a2.size() as usize];
	a2.public_decrypt(&a1, &mut buf, Padding::PKCS1).unwrap();

	// openssl doesn't remove padding automatically
	// this code will remove all trailing 0 bytes in buf
	let mut i = buf.len() - 1;
	while i > 0 && buf[i] == 0 {
		i = i - 1;
	}
	buf.truncate(i + 1);
	return buf
}
fn aenc(a1: bytes, a2: &pkey) -> bytes {
	let mut buf = vec![0; a2.size() as usize];
	a2.public_encrypt(&a1, &mut buf, Padding::PKCS1);
	return buf
}
fn adec(a1: bytes, a2: &skey) -> bytes {
	let mut buf = vec![0; a2.size() as usize];
	a2.private_decrypt(&a1, &mut buf, Padding::PKCS1);

	// openssl doesn't remove padding automatically
	// this code will remove all trailing 0 bytes in buf
	let mut i = buf.len() - 1;
	while i > 0 && buf[i] == 0 {
		i = i - 1;
	}
	buf.truncate(i + 1);
	return buf
}
fn staenc(a1: bytes, a2: &stpkey) -> bytes {
	let mut rng = OsRng;
	let padding = PaddingScheme::new_pkcs1v15_encrypt();
	return a2.encrypt(&mut rng, padding, &a1[..]).expect("failed to encrypt")
}
fn stadec(a1: bytes, a2: &stskey) -> bytes {
	let padding = PaddingScheme::new_pkcs1v15_encrypt();
	return a2.decrypt(padding, &a1).expect("failed to decrypt")
}
fn senc(a1: bytes, a2: symkey) -> bytes {
	let mut iv: [u8; 16] = [0; 16];
	let mut rng = OsRng;
	rng.fill_bytes(&mut iv);

	let mut ct = encrypt(Cipher::aes_128_cbc(), &a2, Some(&iv), &a1[..]).unwrap();

	// Put IV before ciphertext
	let mut res = iv.to_vec();
	res.append(&mut ct);
	return res
}
fn sdec(a1: bytes, a2: symkey) -> bytes {
	let mut iv = a1.to_vec();
	let ct = iv.split_off(16);
	return decrypt(Cipher::aes_128_cbc(), &a2, Some(&iv), &ct[..]).unwrap()
}
fn hash(a1: bytes, a2: bytes) -> bytes {
	let mut a = a1.to_vec();
	a.append(&mut a2.to_vec());

	return openhash(MessageDigest::sha256(), &a[..]).unwrap().to_vec();
}
fn genkey(a1: &skey, a2: bytes) -> stskey {
	let a = hash(a2, a1.private_key_to_pem().unwrap());
	let buf: [u8; 32] = a.try_into()
        .unwrap_or_else(|a: Vec<u8>| panic!("Expected a Vec of length {} but it was {}", 32, a.len()));
	let mut rng = StdRng::from_seed(buf);
	return RsaPrivateKey::new(&mut rng, 2048).expect("failed to generate a private key");
}
fn symkeywrapper(a1: symkey) -> bytes {
	return bincode::serialize(&a1).unwrap()
}
fn symkeyunwrapper(a1: bytes) -> symkey {
	return bincode::deserialize(&a1).unwrap()
}
fn bytesbyteswrapper(a1: bytes, a2: bytes) -> bytes {
	return bincode::serialize(&(a1, a2)).unwrap()
}
fn bytesbytesunwrapper(a1: bytes) -> (bytes, bytes) {
	return bincode::deserialize(&a1).unwrap()
}
fn pkeybyteswrapper(a1: stpkey, a2: bytes) -> bytes {
	return bincode::serialize(&(a1, a2)).unwrap()
}
fn pkeybytesunwrapper(a1: bytes) -> (stpkey, bytes) {
	return bincode::deserialize(&a1).unwrap()
}
fn OBT() -> bytes {
	let buf: [u8; 32] = [13; 32];
	return buf.to_vec()
}
fn REF() -> bytes {
	let buf: [u8; 32] = [37; 32];
	return buf.to_vec()
}
fn fresh_skey(bits: u32) -> skey {
	return Rsa::generate(bits).unwrap();
}
fn fresh_pkey() -> pkey {
	let sk = fresh_skey(2048);
	return pk(&sk)
}
fn fresh_symkey() -> symkey {
	let mut buf: [u8; 16] = [0; 16];
	let mut rng = OsRng;
	rng.fill_bytes(&mut buf);
	return buf
}
fn fresh_bytes() -> bytes {
	let mut buf: [u8; 32] = [0; 32];
	let mut rng = OsRng;
	rng.fill_bytes(&mut buf);
	return buf.to_vec()
}
fn fresh_stpkey() -> stpkey {
	let sk = fresh_stskey();
	return stpk(sk)
}
fn fresh_stskey() -> stskey {
	let mut rng = OsRng;
	return RsaPrivateKey::new(&mut rng, 2048).expect("failed to generate a private key");
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_equation_0() {
		let k = fresh_skey(2048);
		let m = fresh_bytes();
		assert_eq!(checksign(sign(m.clone(), &k), &pk(&k)), m);
	}
	#[test]
	fn test_equation_1() {
		let k = fresh_skey(2048);
		let m = fresh_bytes();
		assert_eq!(adec(aenc(m.clone(), &pk(&k)), &k), m);
	}
	#[test]
	fn test_equation_2() {
		let k = fresh_stskey();
		let m = fresh_bytes();
		assert_eq!(stadec(staenc(m.clone(), &stpk(k.clone())), &k), m);
	}
	#[test]
	fn test_equation_3() {
		let k = fresh_symkey();
		let m = fresh_bytes();
		assert_eq!(sdec(senc(m.clone(), k), k), m);
	}
	#[test]
	fn test_equation_4() {
		let k = fresh_symkey();
		assert_eq!(symkeyunwrapper(symkeywrapper(k.clone())), k);
	}
	
	#[test]
	fn test_equation_5() {
		let b1 = fresh_bytes();
		let b2 = fresh_bytes();
		assert_eq!(bytesbytesunwrapper(bytesbyteswrapper(b1.clone(), b2.clone())), (b1, b2));
	}
	
	#[test]
	fn test_equation_6() {
		let b = fresh_bytes();
		let k = fresh_stpkey();
		assert_eq!(pkeybytesunwrapper(pkeybyteswrapper(k.clone(), b.clone())), (k, b));
	}
}

fn alice(c_AliceTPM: Chan<(), AliceTPM>, c_AliceParent: Chan<(), AliceParent>, tpmencpk: &pkey, tpmsignpk: &pkey) {
	let (c_AliceTPM, pcr) = recv(c_AliceTPM);
	let na = fresh_bytes();
	let sesk_a = fresh_symkey();
	let c_AliceTPM = send(c_AliceTPM, aenc(symkeywrapper(sesk_a), tpmencpk));
	let (c_AliceTPM, sesid_a) = recv(c_AliceTPM);
	let c_AliceTPM = send(c_AliceTPM, senc(bytesbyteswrapper(na.to_vec(), sesid_a), sesk_a));
	let h_obt = hash(OBT(), hash(na.to_vec(), pcr));
	let c_AliceTPM = send(c_AliceTPM, h_obt.to_vec());
	let (c_AliceTPM, sign_obtk) = recv(c_AliceTPM);
	let (obtk, v0) = pkeybytesunwrapper(checksign(sign_obtk, tpmsignpk));
	if h_obt == v0 {
		let secr = fresh_bytes();
		println!("Alice secret: {:?}", secr);
		let c_AliceParent = send(c_AliceParent, staenc(secr, &obtk));
		close(c_AliceParent);
		close(c_AliceTPM);
	};
}

fn parent(c_ParentAlice: Chan<(), ParentAlice>, c_ParentTPM: Chan<(), ParentTPM>, tpmencpk: &pkey) {
	let (c_ParentAlice, envlp) = recv(c_ParentAlice);
	println!("Parent envelope: {:?}", envlp);
	let sesk_p = fresh_symkey();
	let c_ParentTPM = send(c_ParentTPM, aenc(symkeywrapper(sesk_p), tpmencpk));
	let (c_ParentTPM, sesid_p) = recv(c_ParentTPM);
	let c_ParentTPM = send(c_ParentTPM, envlp);

	let c_ParentTPM = c_ParentTPM.sel2();
	let (c_ParentTPM, secr) = recv(c_ParentTPM);
	println!("Parent secret: {:?}", secr);
	close(c_ParentTPM);
	close(c_ParentAlice);
}

fn tpm(c_TPMAlice: Chan<(), TPMAlice>, c_TPMParent: Chan<(), TPMParent>, tpmencsk: &skey, tpmsignsk: &skey) {
	let pcr = fresh_bytes();
	let c_TPMAlice = send(c_TPMAlice, pcr.to_vec());
	let (c_TPMAlice, enc_sesk_a) = recv(c_TPMAlice);
	let sesk_a = symkeyunwrapper(adec(enc_sesk_a, tpmencsk));
	let sesid_a = fresh_bytes();
	let c_TPMAlice = send(c_TPMAlice, sesid_a.to_vec());
	let (c_TPMAlice, n_sesid_a) = recv(c_TPMAlice);
	let (na, v1) = bytesbytesunwrapper(sdec(n_sesid_a, sesk_a));
	if &sesid_a == &v1 {
		let pcr = hash(na, pcr.to_vec());
		let (c_TPMAlice, h_obt) = recv(c_TPMAlice);
		let c_TPMAlice = send(c_TPMAlice, sign(pkeybyteswrapper(stpk(genkey(tpmencsk, h_obt.to_vec())), h_obt), tpmsignsk));
		let (c_TPMParent, enc_sesk_p) = recv(c_TPMParent);
		let sesid_p = fresh_bytes();
		let c_TPMParent = send(c_TPMParent, sesid_p);
		let (c_TPMParent, envlp) = recv(c_TPMParent);
		println!("TPM envelope: {:?}", envlp);
		match c_TPMParent.offer() {
			Left(c_TPMParent) => {
				let pcr = hash(REF(), pcr);
				let c_TPMParent = send(c_TPMParent, sign(bytesbyteswrapper(pcr, envlp), tpmsignsk));
				close(c_TPMParent);
				close(c_TPMAlice);
			},
			Right(c_TPMParent) => {
				let pcr = hash(OBT(), pcr);
				let c_TPMParent = send(c_TPMParent, stadec(envlp, &genkey(tpmencsk, pcr)));
				close(c_TPMParent);
				close(c_TPMAlice);
			}
		}
	};
}

fn main() {
	let tpmencsk = fresh_skey(2048);
	let tpmsignsk = fresh_skey(4096);
	let tpmsignpk = pk(&tpmsignsk);
	let tpmencpk = pk(&tpmencsk);
	let tpmencpk2 = tpmencpk.clone();

	let (c_ParentTPM, c_TPMParent) = session_channel();
	let (c_AliceParent, c_ParentAlice) = session_channel();
	let (c_TPMAlice, c_AliceTPM) = session_channel();

	let alice_t = thread::spawn(move || alice(c_AliceTPM, c_AliceParent, &tpmencpk, &tpmsignpk));
	let parent_t = thread::spawn(move || parent(c_ParentAlice, c_ParentTPM, &tpmencpk2));
	let tpm_t = thread::spawn(move || tpm(c_TPMAlice, c_TPMParent, &tpmencsk, &tpmsignsk));
	let _ = (alice_t.join(), parent_t.join(), tpm_t.join());
}