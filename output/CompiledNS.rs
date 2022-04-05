extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
use std::process;
use std::borrow::Borrow;
use std::marker::PhantomData;
use serde::de::DeserializeOwned;

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

type BA = Recv<Repr<bytes>, Send<Repr<bytes>, Recv<Repr<bytes>, Eps>>>;

type bytes = /* unimplemented */;
type symkey = /* unimplemented */;
type skey = /* unimplemented */;
type pkey = /* unimplemented */;

fn pk(a1: skey) -> pkey {
	unimplemented!();
}
fn aenc(a1: pkey, a2: bytes) -> bytes {
	unimplemented!();
}
fn adec(a1: skey, a2: bytes) -> bytes {
	unimplemented!();
}
fn hash(a1: bytes) -> bytes {
	unimplemented!();
}
fn nawrap(a1: pkey, a2: bytes) -> bytes {
	unimplemented!();
}
fn naunwrap(a1: bytes) -> (pkey, bytes) {
	unimplemented!();
}
fn nanbwrap(a1: pkey, a2: bytes, a3: bytes) -> bytes {
	unimplemented!();
}
fn nanbunwrap(a1: bytes) -> (pkey, bytes, bytes) {
	unimplemented!();
}
fn fresh_bytes() -> bytes {
	unimplemented!();
}
fn fresh_symkey() -> symkey {
	unimplemented!();
}
fn fresh_skey() -> skey {
	unimplemented!();
}
fn fresh_pkey() -> pkey {
	unimplemented!();
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_equation_0() {
		let k = fresh_skey();
		let m = fresh_bytes();
		assert_eq!(adec(k, aenc(pk(k), m)), m);
	}
	#[test]
	fn test_equation_1() {
		let nonce = fresh_bytes();
		let pk = fresh_pkey();
		assert_eq!(naunwrap(nawrap(pk, nonce)), (pk, nonce));
	}
	#[test]
	fn test_equation_2() {
		let nonce1 = fresh_bytes();
		let nonce2 = fresh_bytes();
		let pk = fresh_pkey();
		assert_eq!(nanbunwrap(nanbwrap(pk, nonce1, nonce2)), (pk, nonce1, nonce2));
	}
}

fn a(c_AB: Chan<(), AB>, pkb: pkey, ska: skey) {
	let na = fresh_bytes();
	let ct = aenc(pkb, nawrap(pk(ska), na));
	let c_AB = send(c_AB, ct);
	let (c_AB, ctb) = recv(c_AB);
	let (v0, x_na, x_nb) = nanbunwrap(adec(ska, ctb));
	if pkb == v0 {
		let enc_nb = fresh_bytes();
		if x_na == na {
			let enc_nb = aenc(pkb, x_nb);
			let c_AB = send(c_AB, enc_nb);
			close(c_AB);
		} else {
			let c_AB = send(c_AB, enc_nb);
			close(c_AB);
		};
	};
}

fn b(c_BA: Chan<(), BA>, pka: pkey, skb: skey) {
	let (c_BA, cta) = recv(c_BA);
	let (v1, na) = naunwrap(adec(skb, cta));
	if pka == v1 {
		let nb = fresh_bytes();
		let ct = aenc(pka, nanbwrap(pk(skb), na, nb));
		let c_BA = send(c_BA, ct);
		let (c_BA, z) = recv(c_BA);
		let z_nb = adec(skb, z);
		if z_nb == nb {
			close(c_BA);
		} else {
			close(c_BA);
		};
	};
}

fn main() {
	let ska = fresh_skey();
	let skb = fresh_skey();
	let pkb = pk(skb);
	let pka = pk(ska);

	let (c_AB, c_BA) = session_channel();

	let a_t = thread::spawn(move || a(c_AB, ska, pkb));
	let b_t = thread::spawn(move || b(c_BA, skb, pka));
	let _ = (a_t.join(), b_t.join());
}