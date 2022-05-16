extern crate session_types;
use session_types::*;
use std::{marker};
use serde::{Serialize, Deserialize};
use std::thread;
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

type SourceCourt = Send<Repr<bytes>, Eps>;
type SourcePublic = Send<Repr<PKIBEFORM>, Send<Repr<bytes>, Eps>>;

type CourtAuditor = Send<Repr<bytes>, Eps>;
type CourtDecryptor = Send<Repr<bytes>, Eps>;
type CourtJudge = Offer<Recv<Repr<bytes>, Eps>, Recv<Repr<bytes>, Eps>>;
type CourtSource = Recv<Repr<bytes>, Eps>;
type CourtPublic = Send<Repr<PKFORM>, Eps>;

type DecryptorCourt = Recv<Repr<bytes>, Eps>;
type DecryptorPublic = Send<Repr<PKFORM>, Eps>;

type AuditorCourt = Recv<Repr<bytes>, Eps>;
type AuditorPublic = Send<Repr<PKFORM>, Eps>;

type JudgeCourt = Choose<Send<Repr<bytes>, Eps>, Send<Repr<bytes>, Eps>>;

type PublicAuditor = Recv<Repr<PKFORM>, Recv<Repr<(bytes, bytes)>, Recv<Repr<(bytes, bytes, bytes)>, Eps>>>;
type PublicDecryptor = Recv<Repr<PKFORM>, Eps>;
type PublicCourt = Recv<Repr<PKFORM>, Eps>;
type PublicSource = Recv<Repr<PKIBEFORM>, Recv<Repr<bytes>, Eps>>;

type bytes = /* unimplemented */;
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

fn OK() -> bytes {
	unimplemented!();
}
fn APPROVE() -> bytes {
	unimplemented!();
}
fn REJECT() -> bytes {
	unimplemented!();
}
fn ZKPFAIL() -> bytes {
	unimplemented!();
}
fn pk(a1: skey) -> pkey {
	unimplemented!();
}
fn enc(a1: pkey, a2: bytes, a3: rand) -> bytes {
	unimplemented!();
}
fn dec(a1: skey, a2: bytes) -> bytes {
	unimplemented!();
}
fn sign(a1: skey, a2: bytes) -> bytes {
	unimplemented!();
}
fn checksign(a1: bytes, a2: pkey) -> bytes {
	unimplemented!();
}
fn pkibe(a1: mskeyi) -> mpkeyi {
	unimplemented!();
}
fn encibe(a1: mpkeyi, a2: bytes, a3: bytes) -> bytes {
	unimplemented!();
}
fn extract(a1: mskeyi, a2: bytes) -> skeyi {
	unimplemented!();
}
fn decibe(a1: bytes, a2: skeyi) -> bytes {
	unimplemented!();
}
fn blind(a1: bytes, a2: rand) -> bytes {
	unimplemented!();
}
fn pextract(a1: mskeyi, a2: bytes) -> bytes {
	unimplemented!();
}
fn bextract(a1: rand, a2: bytes) -> skeyi {
	unimplemented!();
}
fn pzk(a1: bytes, a2: rand) -> bytes {
	unimplemented!();
}
fn checkzkpok(a1: bytes, a2: bytes) -> bytes {
	unimplemented!();
}
fn pzkenc(a1: bytes, a2: rand) -> bytes {
	unimplemented!();
}
fn checkzkpokenc(a1: bytes, a2: bytes) -> bytes {
	unimplemented!();
}
fn encwrap(a1: bytes, a2: rand) -> bytes {
	unimplemented!();
}
fn encunwrap(a1: bytes) -> (bytes, rand) {
	unimplemented!();
}
fn signwrap(a1: bytes, a2: bytes, a3: bytes) -> bytes {
	unimplemented!();
}
fn signunwrap(a1: bytes) -> (bytes, bytes, bytes) {
	unimplemented!();
}
fn fresh_bytes() -> bytes {
	unimplemented!();
}
fn fresh_pkey() -> pkey {
	unimplemented!();
}
fn fresh_skey() -> skey {
	unimplemented!();
}
fn fresh_mpkeyi() -> mpkeyi {
	unimplemented!();
}
fn fresh_mskeyi() -> mskeyi {
	unimplemented!();
}
fn fresh_skeyi() -> skeyi {
	unimplemented!();
}
fn fresh_rand() -> rand {
	unimplemented!();
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_equation_0() {
		let k = fresh_skey();
		let m = fresh_bytes();
		let r = fresh_rand();
		assert_eq!(dec(k, enc(pk(k), m, r)), m);
	}
	#[test]
	fn test_equation_1() {
		let m = fresh_bytes();
		let ssk = fresh_skey();
		assert_eq!(checksign(sign(ssk, m), pk(ssk)), m);
	}
	#[test]
	fn test_equation_2() {
		let idi = fresh_bytes();
		let m = fresh_bytes();
		let msk = fresh_mskeyi();
		assert_eq!(decibe(encibe(pkibe(msk), idi, m), extract(msk, idi)), m);
	}
	#[test]
	fn test_equation_3() {
		let idi = fresh_bytes();
		let msk = fresh_mskeyi();
		let r = fresh_rand();
		assert_eq!(bextract(r, pextract(msk, blind(idi, r))), extract(msk, idi));
	}
	#[test]
	fn test_equation_4() {
		let idi = fresh_bytes();
		let r = fresh_rand();
		assert_eq!(checkzkpok(blind(idi, r), pzk(idi, r)), OK());
	}
	#[test]
	fn test_equation_5() {
		let idi = fresh_bytes();
		let r = fresh_rand();
		let sk = fresh_skey();
		assert_eq!(checkzkpokenc(enc(pk(sk), idi, r), pzkenc(idi, r)), OK());
	}
	#[test]
	fn test_equation_6() {
		let id = fresh_bytes();
		let r = fresh_rand();
		assert_eq!(encunwrap(encwrap(id, r)), (id, r));
	}
	#[test]
	fn test_equation_7() {
		let a = fresh_bytes();
		let c = fresh_bytes();
		let z = fresh_bytes();
		assert_eq!(signunwrap(signwrap(a, c, z)), (a, c, z));
	}
}

fn source(c_SourcePublic: Chan<(), SourcePublic>, c_SourceCourt: Chan<(), SourceCourt>, mpk: mpkeyi, id: bytes, msg: bytes) {
	let c_SourcePublic = send(c_SourcePublic, PKIBEFORM(mpk));
	let ct = encibe(mpk, id, msg);
	let c_SourceCourt = send(c_SourceCourt, ct);
	let c_SourcePublic = send(c_SourcePublic, ct);
	close(c_SourceCourt);
	close(c_SourcePublic);
}

fn court(c_CourtPublic: Chan<(), CourtPublic>, c_CourtJudge: Chan<(), CourtJudge>, c_CourtDecryptor: Chan<(), CourtDecryptor>, c_CourtAuditor: Chan<(), CourtAuditor>, c_CourtSource: Chan<(), CourtSource>, pka: pkey, skc: skey, id: bytes) {
	let c_CourtPublic = send(c_CourtPublic, PKFORM(pka));
	let (c_CourtSource, ct) = recv(c_CourtSource);
	match c_CourtJudge.offer() {
		Left(c_CourtJudge) => {
			let (c_CourtJudge, status) = recv(c_CourtJudge);
			let c_CourtDecryptor = send(c_CourtDecryptor, status);
			let c_CourtAuditor = send(c_CourtAuditor, status);
			if status == REJECT() {
				close(c_CourtSource);
				close(c_CourtAuditor);
				close(c_CourtDecryptor);
				close(c_CourtJudge);
				close(c_CourtPublic);
			} else {
				let r = fresh_rand();
				let ctc = enc(pka, id, r);
				let signed_ctc = sign(skc, ctc);
				let c_CourtAuditor = send(c_CourtAuditor, signed_ctc);
				let r2 = fresh_rand();
				let audit = enc(pka, encwrap(id, r), r2);
				let signed_audit = sign(skc, audit);
				let b = blind(id, r);
				let c_CourtDecryptor = send(c_CourtDecryptor, (b, pzk(id, r), signed_ctc, signed_audit, pzkenc(id, r)));
				let (c_CourtDecryptor, status) = recv(c_CourtDecryptor);
				if status == ZKPFAIL() {
					close(c_CourtSource);
					close(c_CourtAuditor);
					close(c_CourtDecryptor);
					close(c_CourtJudge);
					close(c_CourtPublic);
				} else {
					let (c_CourtDecryptor, partial) = recv(c_CourtDecryptor);
					let sk = bextract(r, partial);
					let m = decibe(ct, sk);
					close(c_CourtSource);
					close(c_CourtAuditor);
					close(c_CourtDecryptor);
					close(c_CourtJudge);
					close(c_CourtPublic);
				};
			};
		},
		Right(c_CourtJudge) => {
			let (c_CourtJudge, status) = recv(c_CourtJudge);
			let c_CourtDecryptor = send(c_CourtDecryptor, status);
			let c_CourtAuditor = send(c_CourtAuditor, status);
			if status == REJECT() {
				close(c_CourtSource);
				close(c_CourtAuditor);
				close(c_CourtDecryptor);
				close(c_CourtJudge);
				close(c_CourtPublic);
			} else {
				let r = fresh_rand();
				let ctc = enc(pka, id, r);
				let signed_ctc = sign(skc, ctc);
				let c_CourtAuditor = send(c_CourtAuditor, signed_ctc);
				let r2 = fresh_rand();
				let audit = enc(pka, encwrap(id, r), r2);
				let signed_audit = sign(skc, audit);
				let b = blind(id, r);
				let c_CourtDecryptor = send(c_CourtDecryptor, (b, pzk(id, r), signed_ctc, signed_audit, pzkenc(id, r)));
				let (c_CourtDecryptor, status) = recv(c_CourtDecryptor);
				if status == ZKPFAIL() {
					close(c_CourtSource);
					close(c_CourtAuditor);
					close(c_CourtDecryptor);
					close(c_CourtJudge);
					close(c_CourtPublic);
				} else {
					let (c_CourtDecryptor, partial) = recv(c_CourtDecryptor);
					let sk = bextract(r, partial);
					let m = decibe(ct, sk);
					close(c_CourtSource);
					close(c_CourtAuditor);
					close(c_CourtDecryptor);
					close(c_CourtJudge);
					close(c_CourtPublic);
				};
			};
		}
	}
}

fn decryptor(c_DecryptorPublic: Chan<(), DecryptorPublic>, c_DecryptorCourt: Chan<(), DecryptorCourt>, c_DecryptorAuditor: Chan<(), DecryptorAuditor>, pkc: pkey, skd: skey, msk: mskeyi) {
	let c_DecryptorPublic = send(c_DecryptorPublic, PKFORM(pkc));
	let (c_DecryptorCourt, status) = recv(c_DecryptorCourt);
	if status == REJECT() {
		close(c_DecryptorAuditor);
		close(c_DecryptorCourt);
		close(c_DecryptorPublic);
	} else {
		let (c_DecryptorCourt, drequest) = recv(c_DecryptorCourt);
		let (b, pzk, signed_ctc, signed_audit, pzkenc) = drequest;
		let ctc = checksign(signed_ctc, pkc);
		let audit = checksign(signed_audit, pkc);
		if checkzkpokenc(ctc, pzkenc) == OK() && checkzkpok(b, pzk) == OK() {
			let status = APPROVE();
			let c_DecryptorCourt = send(c_DecryptorCourt, status);
			let c_DecryptorAuditor = send(c_DecryptorAuditor, status);
			if status == ZKPFAIL() {
				close(c_DecryptorAuditor);
				close(c_DecryptorCourt);
				close(c_DecryptorPublic);
			} else {
				let partial = pextract(msk, b);
				let dsign = sign(skd, signwrap(audit, ctc, pzkenc));
				let c_DecryptorCourt = send(c_DecryptorCourt, partial);
				let c_DecryptorAuditor = send(c_DecryptorAuditor, dsign);
				close(c_DecryptorAuditor);
				close(c_DecryptorCourt);
				close(c_DecryptorPublic);
			};
		} else {
			let status = ZKPFAIL();
			let c_DecryptorCourt = send(c_DecryptorCourt, status);
			let c_DecryptorAuditor = send(c_DecryptorAuditor, status);
			if status == ZKPFAIL() {
				close(c_DecryptorAuditor);
				close(c_DecryptorCourt);
				close(c_DecryptorPublic);
			} else {
				let partial = pextract(msk, b);
				let dsign = sign(skd, signwrap(audit, ctc, pzkenc));
				let c_DecryptorCourt = send(c_DecryptorCourt, partial);
				let c_DecryptorAuditor = send(c_DecryptorAuditor, dsign);
				close(c_DecryptorAuditor);
				close(c_DecryptorCourt);
				close(c_DecryptorPublic);
			};
		};
	};
}

fn auditor(c_AuditorPublic: Chan<(), AuditorPublic>, c_AuditorCourt: Chan<(), AuditorCourt>, c_AuditorDecryptor: Chan<(), AuditorDecryptor>, pkd: pkey, pkc: pkey, ska: skey) {
	let c_AuditorPublic = send(c_AuditorPublic, PKFORM(pkd));
	let (c_AuditorCourt, status) = recv(c_AuditorCourt);
	if status == REJECT() {
		close(c_AuditorDecryptor);
		close(c_AuditorCourt);
		close(c_AuditorPublic);
	} else {
		let (c_AuditorCourt, ctcsign) = recv(c_AuditorCourt);
		let (c_AuditorDecryptor, status) = recv(c_AuditorDecryptor);
		if status == ZKPFAIL() {
			close(c_AuditorDecryptor);
			close(c_AuditorCourt);
			close(c_AuditorPublic);
		} else {
			let (c_AuditorDecryptor, dsign) = recv(c_AuditorDecryptor);
			let c_AuditorPublic = send(c_AuditorPublic, (ctcsign, dsign));
			let ctc = checksign(ctcsign, pkc);
			let id_ctc = dec(ska, ctc);
			let (audit, ctcd, zkenc) = signunwrap(checksign(dsign, pkd));
			let (id_audit, r) = encunwrap(dec(ska, audit));
			if ctc == ctcd {
				let c_AuditorPublic = send(c_AuditorPublic, (audit, ctcd, zkenc));
				close(c_AuditorDecryptor);
				close(c_AuditorCourt);
				close(c_AuditorPublic);
			} else {
				let c_AuditorPublic = send(c_AuditorPublic, (audit, ctcd, zkenc));
				close(c_AuditorDecryptor);
				close(c_AuditorCourt);
				close(c_AuditorPublic);
			};
		};
	};
}

fn judge(c_JudgeCourt: Chan<(), JudgeCourt>) {
	let status = APPROVE();
	// Need to make a choice on c_JudgeCourt. Either c_JudgeCourt.sel1() or c_JudgeCourt.sel2()
	/*
	let c_JudgeCourt = c_JudgeCourt.sel1();
	let c_JudgeCourt = send(c_JudgeCourt, status);
	close(c_JudgeCourt);
	*/

	/*
	let c_JudgeCourt = c_JudgeCourt.sel2();
	let status = REJECT();
	let c_JudgeCourt = send(c_JudgeCourt, status);
	close(c_JudgeCourt);
	*/
}

fn public(c_PublicSource: Chan<(), PublicSource>, c_PublicCourt: Chan<(), PublicCourt>, c_PublicDecryptor: Chan<(), PublicDecryptor>, c_PublicAuditor: Chan<(), PublicAuditor>) {
	let (c_PublicSource, mpkform) = recv(c_PublicSource);
	let (c_PublicCourt, pkaform) = recv(c_PublicCourt);
	let (c_PublicDecryptor, pkcform) = recv(c_PublicDecryptor);
	let (c_PublicAuditor, pkdform) = recv(c_PublicAuditor);
	let (c_PublicSource, ct) = recv(c_PublicSource);
	let (c_PublicAuditor, auditableproof1) = recv(c_PublicAuditor);
	let (c_PublicAuditor, auditableproof2) = recv(c_PublicAuditor);
	close(c_PublicAuditor);
	close(c_PublicDecryptor);
	close(c_PublicCourt);
	close(c_PublicSource);
}

fn main() {
	let id = fresh_bytes();
	let msg = fresh_bytes();
	let msk = fresh_mskeyi();
	let ska = fresh_skey();
	let skc = fresh_skey();
	let skd = fresh_skey();
	let pkd = pk(skd);
	let pkc = pk(skc);
	let pka = pk(ska);
	let mpk = pkibe(msk);

	let (c_SourceCourt, c_CourtSource) = session_channel();
	let (c_DecryptorAuditor, c_AuditorDecryptor) = session_channel();
	let (c_CourtAuditor, c_AuditorCourt) = session_channel();
	let (c_CourtDecryptor, c_DecryptorCourt) = session_channel();
	let (c_JudgeCourt, c_CourtJudge) = session_channel();
	let (c_AuditorPublic, c_PublicAuditor) = session_channel();
	let (c_DecryptorPublic, c_PublicDecryptor) = session_channel();
	let (c_CourtPublic, c_PublicCourt) = session_channel();
	let (c_SourcePublic, c_PublicSource) = session_channel();

	let source_t = thread::spawn(move || source(c_SourcePublic, c_SourceCourt, mpk, id, msg));
	let court_t = thread::spawn(move || court(c_CourtPublic, c_CourtJudge, c_CourtDecryptor, c_CourtAuditor, c_CourtSource, pka, skc, id));
	let decryptor_t = thread::spawn(move || decryptor(c_DecryptorPublic, c_DecryptorCourt, c_DecryptorAuditor, pkc, skd, msk));
	let auditor_t = thread::spawn(move || auditor(c_AuditorPublic, c_AuditorCourt, c_AuditorDecryptor, pkd, pkc, ska));
	let judge_t = thread::spawn(move || judge(c_JudgeCourt));
	let public_t = thread::spawn(move || public(c_PublicSource, c_PublicCourt, c_PublicDecryptor, c_PublicAuditor));
	let _ = (source_t.join(), court_t.join(), decryptor_t.join(), auditor_t.join(), judge_t.join(), public_t.join());
}