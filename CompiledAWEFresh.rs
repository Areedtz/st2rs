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

type Source = Recv<Repr<PKIBEFORM>, Send<Repr<T>, Send<Repr<T>, Eps>>>;
type Court = Recv<Repr<SKFORM>, Recv<Repr<PKFORM>, Recv<Repr<T>, Send<Repr<T>, Send<Repr<(T, T, T, T, T)>, Recv<Repr<T>, Eps>>>>>>;
type Decryptor = Recv<Repr<SKIBEFORM>, Recv<Repr<SKFORM>, Recv<Repr<PKFORM>, Recv<Repr<(T, T, T, T, T)>, Send<Repr<T>, Send<Repr<T>, Eps>>>>>>;
type Auditor = Recv<Repr<SKFORM>, Recv<Repr<PK2FORM>, Recv<Repr<T>, Recv<Repr<T>, Send<Repr<(T, T)>, Send<Repr<(T, T, T)>, Eps>>>>>>;
type KeyDistributor = Send<Repr<SKFORM>, Send<Repr<PK2FORM>, Send<Repr<SKFORM>, Send<Repr<PKFORM>, Send<Repr<SKIBEFORM>, Send<Repr<SKFORM>, Send<Repr<PKFORM>, Send<Repr<PKIBEFORM>, Send<Repr<PKIBEFORM>, Send<Repr<PKFORM>, Send<Repr<PKFORM>, Send<Repr<PKFORM>, Eps>>>>>>>>>>>>;
type Public = Recv<Repr<PKIBEFORM>, Recv<Repr<PKFORM>, Recv<Repr<PKFORM>, Recv<Repr<PKFORM>, Recv<Repr<T>, Recv<Repr<(T, T)>, Recv<Repr<(T, T, T)>, Eps>>>>>>>;



type pkey = /* unimplemented */;
type skey = /* unimplemented */;
type mpkeyi = /* unimplemented */;
type mskeyi = /* unimplemented */;
type skeyi = /* unimplemented */;
type rand = /* unimplemented */;
type bitstring = /* unimplemented */;

#[derive(Serialize, Deserialize)]
struct SKIBEFORM(mskeyi);

#[derive(Serialize, Deserialize)]
struct PKIBEFORM(mpkeyi);

#[derive(Serialize, Deserialize)]
struct PKFORM(pkey);

#[derive(Serialize, Deserialize)]
struct PK2FORM(pkey, pkey);

#[derive(Serialize, Deserialize)]
struct SKFORM(skey);

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
fn fresh_bitstring() -> bitstring {
unimplemented!()
}

fn source(c: Chan<(), Source>, id: T, msg: T) {
let (c, mpkform) = recv(c);
let PKIBEFORM(mpk) = mpkform;
let ct = encibe(mpk,id,msg);
let c = send(c,ct);
let c = send(c,ct);
close(c)
}

fn court(c: Chan<(), Court>, id: T) {
let (c,skcform) = recv(c);
let (c,pkaform) = recv(c);
let (c, ct) = recv(c);
let SKFORM(skc) = skcform;
let PKFORM(pka) = pkaform;
let r = fresh_rand();
let ctc = enc(pka,id,r);
let signed_ctc = sign(skc,ctc);
let c = send(c,signed_ctc);
let r2 = fresh_rand();
let audit = enc(pka,(id,r),r2);
let signed_audit = sign(skc,audit);
let b = blind(id,r);
let c = send(c,(b,pzk(id,r),signed_ctc,signed_audit,pzkenc(id,r)));
let (c,partial) = recv(c);
let sk = bextract(r,partial);
let m = decibe(ct,sk);
close(c)
}

fn decryptor(c: Chan<(), Decryptor>) {
let (c,mskform) = recv(c);
let (c,skdform) = recv(c);
let (c,pkcform) = recv(c);
let (c, drequest) = recv(c);
let SKIBEFORM(msk) = mskform;
let SKFORM(skd) = skdform;
let PKFORM(pkc) = pkcform;
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

fn auditor(c: Chan<(), Auditor>) {
let (c,skaform) = recv(c);
let (c,pkcdform) = recv(c);
let (c,ctcsign) = recv(c);
let (c,dsign) = recv(c);
let c = send(c,(ctcsign,dsign));
let SKFORM(ska) = skaform;
let PK2FORM(pkc,pkd) = pkcdform;
let ctc = checksign(ctcsign,pkc);
let id_ctc = dec(ska,ctc);
let (audit, ctcd, zkenc) = checksign(dsign,pkd);
let (id_audit, r) = dec(ska,audit);
let c = send(c,(audit,ctcd,zkenc));
close(c)
}

fn keydistributor(c: Chan<(), KeyDistributor>) {
let msk = fresh_mskeyi();
let ska = fresh_skey();
let skc = fresh_skey();
let skd = fresh_skey();
let mpk = pkibe(msk);
let pka = pk(ska);
let pkc = pk(skc);
let pkd = pk(skd);
let c = send(c,SKFORM(ska));
let c = send(c,PK2FORM(pkc,pkd));
let c = send(c,SKFORM(skc));
let c = send(c,PKFORM(pka));
let c = send(c,SKIBEFORM(msk));
let c = send(c,SKFORM(skd));
let c = send(c,PKFORM(pkc));
let c = send(c,PKIBEFORM(mpk));
let c = send(c,PKIBEFORM(mpk));
let c = send(c,PKFORM(pka));
let c = send(c,PKFORM(pkc));
let c = send(c,PKFORM(pkd));
close(c)
}

fn public(c: Chan<(), Public>) {
let (c,mpkform) = recv(c);
let (c,pkaform) = recv(c);
let (c,pkcform) = recv(c);
let (c,pkdform) = recv(c);
let (c,ct) = recv(c);
let (c,auditableproof1) = recv(c);
let (c,auditableproof2) = recv(c);
close(c)
}

fn main() {

}
