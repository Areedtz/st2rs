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
use rand::RngCore;
use uid::IdU8;

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

type AB = Send<Repr<Bytes>, Recv<Repr<Bytes>, Send<Repr<Bytes>, Eps>>>;

type BA = Recv<Repr<Bytes>, Send<Repr<Bytes>, Recv<Repr<Bytes>, Eps>>>;

type Id = u8;
type Bytes = Vec<u8>;
type Skey = RsaPrivateKey;
type Pkey = RsaPublicKey;

fn pk(a1: &Skey) -> Pkey {
	return RsaPublicKey::from(a1)
}
fn aenc(a1: &Pkey, a2: Bytes) -> Bytes {
	let mut rng = OsRng;
	let padding = PaddingScheme::new_pkcs1v15_encrypt();
	return a1.encrypt(&mut rng, padding, &a2[..]).expect("failed to encrypt")
}
fn adec(a1: &Skey, a2: Bytes) -> Bytes {
	let padding = PaddingScheme::new_pkcs1v15_encrypt();
	return a1.decrypt(padding, &a2).expect("failed to decrypt")
}
fn nawrap(a1: Id, a2: &Bytes) -> Bytes {
	return bincode::serialize(&(a1, a2)).unwrap()
}
fn naunwrap(a1: Bytes) -> (Id, Bytes) {
	return bincode::deserialize(&a1).unwrap()
}
fn nanbwrap(a1: Id, a2: &Bytes, a3: &Bytes) -> Bytes {
	return bincode::serialize(&(a1, a2, a3)).unwrap()
}
fn nanbunwrap(a1: Bytes) -> (Id, Bytes, Bytes) {
	return bincode::deserialize(&a1).unwrap()
}
fn fresh_id() -> Id {
	return IdU8::<u8>::new().get()
}
fn fresh_bytes() -> Bytes {
	let mut buf: [u8; 32] = [0; 32];
	let mut rng = OsRng;
	rng.fill_bytes(&mut buf);
	return buf.to_vec()
}
fn fresh_skey() -> Skey {
	println!("Generating skey");
	let mut rng = OsRng;
	return RsaPrivateKey::new(&mut rng, 2048).expect("failed to generate a private key");
}
fn fresh_pkey() -> Pkey {
	let sk = fresh_skey();
	return pk(&sk)
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_equation_0() {
		let k = fresh_skey();
		let m = fresh_bytes();
		assert_eq!(adec(&k, aenc(&pk(&k), m.clone())), m);
	}
	#[test]
	fn test_equation_1() {
		let idp = fresh_id();
		let nonce = fresh_bytes();
		assert_eq!(naunwrap(nawrap(idp, &nonce)), (idp, nonce));
	}
	#[test]
	fn test_equation_2() {
		let idp = fresh_id();
		let nonce1 = fresh_bytes();
		let nonce2 = fresh_bytes();
		assert_eq!(nanbunwrap(nanbwrap(idp, &nonce1, &nonce2)), (idp, nonce1, nonce2));
	}
}

fn a(c_AB: Chan<(), AB>, pkb: &Pkey, ska: &Skey, idb: Id, ida: Id) {
	let na = fresh_bytes();
	println!("A start");
	let ct = aenc(pkb, nawrap(ida, &na));
	let c_AB = send(c_AB, ct);
	let (c_AB, ctb) = recv(c_AB);
	let (v0, x_na, x_nb) = nanbunwrap(adec(ska, ctb));
	if idb == v0 {
		let enc_nb = fresh_bytes();
		println!("A mid");
		if *x_na == na {
			let enc_nb = aenc(pkb, x_nb);
			let c_AB = send(c_AB, enc_nb);
			println!("A end");
			close(c_AB);
		} else {
			let c_AB = send(c_AB, enc_nb);
			println!("A end (for ELSE)");
			close(c_AB);
		};
	};
}

fn b(c_BA: Chan<(), BA>, pka: &Pkey, skb: &Skey, ida: Id, idb: Id) {
	let (c_BA, cta) = recv(c_BA);
	let (v1, na) = naunwrap(adec(skb, cta));
	if ida == v1 {
		let nb = fresh_bytes();
		println!("B start");
		let ct = aenc(pka, nanbwrap(idb, &na, &nb));
		let c_BA = send(c_BA, ct);
		let (c_BA, z) = recv(c_BA);
		println!("z = {:?}", z);
		let z_nb = adec(skb, z);
		println!("z_nb = {:?}", z_nb);
		println!("nb = {:?}", nb);
		if *z_nb == nb {
			println!("B end");
			close(c_BA);
		} else {
			println!("B end (for ELSE)");
			close(c_BA);
		};
	};
}

fn main() {
	let ida = fresh_id();
	let idb = fresh_id();
	println!("Generated ids {:?} {:?}", ida, idb);
	let ska = fresh_skey();
	let skb = fresh_skey();
	let pkb = pk(&skb);
	let pka = pk(&ska);
	println!("Generated keys");

	let (c_AB, c_BA) = session_channel();

	let a_t = thread::spawn(move || a(c_AB, &pkb, &ska, idb, ida));
	let b_t = thread::spawn(move || b(c_BA, &pka, &skb, ida, idb));
	let _ = (a_t.join(), b_t.join());
}