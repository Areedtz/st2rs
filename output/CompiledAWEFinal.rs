extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
use std::borrow::Borrow;
use rand::Rng;
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

type Source = Send<Repr<PKIBEFORM>, Send<Repr<T>, Send<Repr<T>, Eps>>>;
type Court = Send<Repr<PKFORM>, Recv<Repr<T>, Send<Repr<T>, Send<Repr<(T, T, T, T, T)>, Recv<Repr<T>, Eps>>>>>;
type Decryptor = Send<Repr<PKFORM>, Recv<Repr<(T, T, T, T, T)>, Send<Repr<T>, Send<Repr<T>, Eps>>>>;
type Auditor = Send<Repr<PKFORM>, Recv<Repr<T>, Recv<Repr<T>, Send<Repr<(T, T)>, Send<Repr<(T, T, T)>, Eps>>>>>;
type Public = Recv<Repr<PKIBEFORM>, Recv<Repr<PKFORM>, Recv<Repr<PKFORM>, Recv<Repr<PKFORM>, Recv<Repr<T>, Recv<Repr<(T, T)>, Recv<Repr<(T, T, T)>, Eps>>>>>>>;



type pkey = /* unimplemented */;
type skey = /* unimplemented */;
type mpkeyi = /* unimplemented */;
type mskeyi = /* unimplemented */;
type skeyi = /* unimplemented */;
type rand = /* unimplemented */;

#[derive(Serialize, Deserialize)]
struct PKIBEFORM(mpkeyi);

#[derive(Serialize, Deserialize)]
struct PKFORM(pkey);

fn OK() -> bitstring {
unimplemented!()
}
fn pk(a1: skey) -> pkey {
unimplemented!()
}
fn enc(a1: pkey, a2: bitstring, a3: rand) -> bitstring {
unimplemented!()
}
fn dec(a1: skey, a2: bitstring) -> bitstring {
unimplemented!()
}
fn sign(a1: skey, a2: bitstring) -> bitstring {
unimplemented!()
}
fn checksign(a1: bitstring, a2: pkey) -> bitstring {
unimplemented!()
}
fn pkibe(a1: mskeyi) -> mpkeyi {
unimplemented!()
}
fn encibe(a1: mpkeyi, a2: bitstring, a3: bitstring) -> bitstring {
unimplemented!()
}
fn extract(a1: mskeyi, a2: bitstring) -> skeyi {
unimplemented!()
}
fn decibe(a1: bitstring, a2: skeyi) -> bitstring {
unimplemented!()
}
fn blind(a1: bitstring, a2: rand) -> bitstring {
unimplemented!()
}
fn pextract(a1: mskeyi, a2: bitstring) -> bitstring {
unimplemented!()
}
fn bextract(a1: rand, a2: bitstring) -> skeyi {
unimplemented!()
}
fn pzk(a1: bitstring, a2: rand) -> bitstring {
unimplemented!()
}
fn checkzkpok(a1: bitstring, a2: bitstring) -> bitstring {
unimplemented!()
}
fn pzkenc(a1: bitstring, a2: rand) -> bitstring {
unimplemented!()
}
fn checkzkpokenc(a1: bitstring, a2: bitstring) -> bitstring {
unimplemented!()
}
fn fresh_pkey() -> pkey {
unimplemented!()
}
fn fresh_skey() -> skey {
unimplemented!()
}
fn fresh_mpkeyi() -> mpkeyi {
unimplemented!()
}
fn fresh_mskeyi() -> mskeyi {
unimplemented!()
}
fn fresh_skeyi() -> skeyi {
unimplemented!()
}
fn fresh_rand() -> rand {
unimplemented!()
}

fn source(c: Chan<(), Source>, mpk: mpkeyi, id: bitstring, msg: bitstring) {
let c = send(c,PKIBEFORM(mpk));
let ct = encibe(mpk,id,msg);
let c = send(c,ct);
let c = send(c,ct);
close(c)
}

fn court(c: Chan<(), Court>, pka: pkey, skc: skey, id: bitstring) {
let c = send(c,PKFORM(pka));
let (c, ct) = recv(c);
let r = fresh_rand();
let ctc = enc(pka,id,r);
let signed_ctc = sign(skc,ctc);
let c = send(c,signed_ctc);
let r2 = fresh_rand();
let audit = enc(pka,(id,r),r2);
let signed_audit = sign(skc,audit);
let b = blind(id,r);
let c = send(c,(b,pzk(id,r),signed_ctc,signed_audit,pzkenc(id,r)));
let (c, partial) = recv(c);
let sk = bextract(r,partial);
let m = decibe(ct,sk);
close(c)
}

fn decryptor(c: Chan<(), Decryptor>, pkc: pkey, skd: skey, msk: mskeyi) {
let c = send(c,PKFORM(pkc));
let (c, drequest) = recv(c);
let (b, pzk, signed_ctc, signed_audit, pzkenc) = drequest;
let ctc = checksign(signed_ctc,pkc);
let audit = checksign(signed_audit,pkc);
let p = fresh_bitstring();
let partial = if checkzkpokenc(ctc,pzkenc) == OK() && checkzkpok(b,pzk) == OK() {
	pextract(msk,b)
} else {
	p
};
let dsign = sign(skd,(audit,ctc,pzkenc));
let c = send(c,partial);
let c = send(c,dsign);
close(c)
}

fn auditor(c: Chan<(), Auditor>, pkd: pkey, pkc: pkey, ska: skey) {
let c = send(c,PKFORM(pkd));
let (c, ctcsign) = recv(c);
let (c, dsign) = recv(c);
let c = send(c,(ctcsign,dsign));
let ctc = checksign(ctcsign,pkc);
let id_ctc = dec(ska,ctc);
let (audit, ctcd, zkenc) = checksign(dsign,pkd);
let (id_audit, r) = dec(ska,audit);
let c = send(c,(audit,ctcd,zkenc));
close(c)
}

fn public(c: Chan<(), Public>) {
let (c, mpkform) = recv(c);
let (c, pkaform) = recv(c);
let (c, pkcform) = recv(c);
let (c, pkdform) = recv(c);
let (c, ct) = recv(c);
let (c, auditableproof1) = recv(c);
let (c, auditableproof2) = recv(c);
close(c)
}
