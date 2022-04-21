#![allow(warnings)]

extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
use std::marker::PhantomData;
use serde::de::DeserializeOwned;
use uid::IdU8;
use std::collections::HashMap;

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

type AB = Send<Repr<bytes>, Recv<Repr<bytes>, Send<Repr<bytes>, Eps>>>;
type APublic = Send<Repr<pkey>, Send<Repr<id>, Recv<Repr<id>, Eps>>>;

type BA = Recv<Repr<bytes>, Send<Repr<bytes>, Recv<Repr<bytes>, Eps>>>;
type BPublic = Send<Repr<pkey>, Eps>;

type PublicB = Recv<Repr<pkey>, Eps>;
type PublicA = Recv<Repr<pkey>, Recv<Repr<id>, Send<Repr<id>, Eps>>>;

type id = u8;
type bytes = Vec<u8>;
type skey = RsaPrivateKey;
type pkey = RsaPublicKey;

let mut id_pkey_storage = HashMap::new();
let mut skey_id_storage = HashMap::new();

fn pk(a1: &skey) -> pkey {
	return RsaPublicKey::from(a1)
}
fn sk2id(a1: skey) -> id {
	return skey_id_storage.get(a1).expect("skey does not exists in storage")
}
fn id2pk(a1: id) -> pkey {
	return id_pkey_storage.get(a1).expect("id does not exists in storage")
}
fn aenc(a1: &pkey, a2: bytes) -> bytes {
	let mut rng = OsRng;
	let padding = PaddingScheme::new_pkcs1v15_encrypt();
	return a1.encrypt(&mut rng, padding, &a2[..]).expect("failed to encrypt")
}
fn adec(a1: &skey, a2: bytes) -> bytes {
	let padding = PaddingScheme::new_pkcs1v15_encrypt();
	return a1.decrypt(padding, &a2).expect("failed to decrypt")
}
fn nawrap(a1: id, a2: &bytes) -> bytes {
	return bincode::serialize(&(a1, a2)).unwrap()
}
fn naunwrap(a1: bytes) -> (id, bytes) {
	return bincode::deserialize(&a1).unwrap()
}
fn nanbwrap(a1: id, a2: &bytes, a3: &bytes) -> bytes {
	return bincode::serialize(&(a1, a2, a3)).unwrap()
}
fn nanbunwrap(a1: bytes) -> (id, bytes, bytes) {
	return bincode::deserialize(&a1).unwrap()
}
fn fresh_id() -> id {
	return IdU8::<u8>::new().get()
}
fn fresh_bytes() -> bytes {
	let mut buf: [u8; 32] = [0; 32];
	let mut rng = OsRng;
	rng.fill_bytes(&mut buf);
	return buf.to_vec()
}
fn fresh_skey() -> skey {
	println!("Generating skey");
	let mut rng = OsRng;
	return RsaPrivateKey::new(&mut rng, 2048).expect("failed to generate a private key");
}
fn fresh_pkey() -> pkey {
	let sk = fresh_skey();
	return pk(&sk)
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_equation_0() {
		let k = fresh_skey();
		assert_eq!(id2pk(sk2id(k)), pk(k));
	}
	#[test]
	fn test_equation_1() {
		let k = fresh_skey();
		let m = fresh_bytes();
		assert_eq!(adec(k, aenc(pk(k), m)), m);
	}
	#[test]
	fn test_equation_2() {
		let idp = fresh_id();
		let nonce = fresh_bytes();
		assert_eq!(naunwrap(nawrap(idp, nonce)), (idp, nonce));
	}
	#[test]
	fn test_equation_3() {
		let idp = fresh_id();
		let nonce1 = fresh_bytes();
		let nonce2 = fresh_bytes();
		assert_eq!(nanbunwrap(nanbwrap(idp, nonce1, nonce2)), (idp, nonce1, nonce2));
	}
}

fn a(c_APublic: Chan<(), APublic>, c_AB: Chan<(), AB>, idb: id, ska: skey) {
	let c_APublic = send(c_APublic, pk(ska));
	let c_APublic = send(c_APublic, sk2id(ska));
	let (c_APublic, idx) = recv(c_APublic);
	let pkx = id2pk(idx);
	let na = fresh_bytes();
	let ida = sk2id(ska);
	println!("A started");
	let ct = aenc(pkx, nawrap(ida, na));
	let c_AB = send(c_AB, ct);
	let (c_AB, ctb) = recv(c_AB);
	let (v0, v1, nx) = nanbunwrap(adec(ska, ctb));
	if idx == v0 && na == v1 {
		let enc_nb = aenc(pkx, nx);
		let c_AB = send(c_AB, enc_nb);
		let pkb = id2pk(idb);
		if pkx == pkb {
			println!("A ended");
			close(c_AB);
			close(c_APublic);
		} else {
			println!("A ended (not talking to B)");
			close(c_AB);
			close(c_APublic);
		};
	};
}

fn b(c_BPublic: Chan<(), BPublic>, c_BA: Chan<(), BA>, ida: id, skb: skey) {
	let c_BPublic = send(c_BPublic, pk(skb));
	let (c_BA, cta) = recv(c_BA);
	let (v2, ny) = naunwrap(adec(skb, cta));
	if ida == v2 {
		let nb = fresh_bytes();
		let idb = sk2id(skb);
		println!("B started");
		let pka = id2pk(ida);
		let ct = aenc(pka, nanbwrap(idb, ny, nb));
		let c_BA = send(c_BA, ct);
		let (c_BA, z) = recv(c_BA);
		let z_nb = adec(skb, z);
		if z_nb == nb {
			println!("B ended");
			close(c_BA);
			close(c_BPublic);
		} else {
			println!("B ended (not talking to A)");
			close(c_BA);
			close(c_BPublic);
		};
	};
}

fn public(c_PublicA: Chan<(), PublicA>, c_PublicB: Chan<(), PublicB>, idb: id) {
	let (c_PublicA, pka) = recv(c_PublicA);
	let (c_PublicA, ida) = recv(c_PublicA);
	let (c_PublicB, pkb) = recv(c_PublicB);
	let c_PublicA = send(c_PublicA, idb);
	close(c_PublicB);
	close(c_PublicA);
}

fn main() {
	let ska = fresh_skey();
	let skb = fresh_skey();

	skey_id_storage.insert(ska, fresh_id());
	skey_id_storage.insert(skb, fresh_id());

	let idb = sk2id(skb);
	let ida = sk2id(ska);

	id_pkey_storage.insert(idb, pk(skb));
	id_pkey_storage.insert(ida, pk(ska));

	let (c_AB, c_BA) = session_channel();
	let (c_BPublic, c_PublicB) = session_channel();
	let (c_APublic, c_PublicA) = session_channel();

	let a_t = thread::spawn(move || a(c_APublic, c_AB, idb, ska));
	let b_t = thread::spawn(move || b(c_BPublic, c_BA, ida, skb));
	let public_t = thread::spawn(move || public(c_PublicA, c_PublicB, idb));
	let _ = (a_t.join(), b_t.join(), public_t.join());
}