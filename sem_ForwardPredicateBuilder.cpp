/*
 *	BackwardPredicateBuilder class
 *	
 *	This file is part of OTAWA
 *	Copyright (c) 2021, IRIT UPS.
 *	
 *	OTAWA is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *	
 *	OTAWA is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *	
 *	You should have received a copy of the GNU General Public License
 *	along with OTAWA; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 *	02110-1301  USA
 */

#include <otawa/pred/predicates.h>
#include <elm/data/HashMap.h>
#include <elm/data/ListMap.h>
#include <elm/data/ListMap.h>
#include <otawa/proc/BBProcessor.h>

namespace otawa { namespace pred {

using namespace sem;

static sem::cond_t symetric(sem::cond_t c) {
	switch(c) {
	case NO_COND:	return c;
	case EQ:		return c;
	case LT:		return GT;
	case LE:		return GE;
	case GE:		return LE;
	case GT:		return LT;
	case ANY_COND:	return c;
	case NE:		return c;
	case ULT:		return UGT;
	case ULE:		return UGE;
	case UGE:		return ULE;
	case UGT:		return ULT;
	default:		ASSERT(false);			
	}
}
	

class ExpressionComparator {
public:
	int doCompare(const Expression * e1, const Expression * e2) const { return e1->compare(e2); }
	static int compare(const Expression * e1, const Expression * e2) { return e1->compare(e2); }
};

class ForwardPredicate {
public:
	ForwardPredicate(const Expression *e1, cond_t c, const Expression * e2)
		: _c(c), _e1(e1), _e2(e2) { }
	ForwardPredicate(const ForwardPredicate& p): _c(p._c), _e1(p._e1), _e2(p._e2) {}
	inline cond_t cond() const { return _c; }
	inline const Expression *arg1() const { return _e1; }
	inline const Expression *arg2() const { return _e2; }
private:
	cond_t _c;
	const Expression *_e1, *_e2;
};
io::Output& operator<<(io::Output& out, ForwardPredicate *p) {
	out << p->arg1() << ' ';
	switch(p->cond()) {
	case NO_COND:	out << "!"; break;
	case EQ:		out << "="; break;
	case LT:		out << "<"; break;
	case LE:		out << "<="; break;
	case GE:		out << ">"; break;
	case GT:		out << ">="; break;
	case ANY_COND:	out << "?"; break;
	case NE:		out << "!="; break;
	case ULT:		out << "<+"; break;
	case ULE:		out << "<=+"; break;
	case UGE:		out << ">+"; break;
	case UGT:		out << ">=+"; break;
	case  MAX_COND:	ASSERT(false);
	}
	out << ' ' << p->arg2();
	return out;
}

class ForwardState {
public:
	
	ForwardState(ExpressionManager& _man): b(false), man(_man) {}

	~ForwardState() {
		for(auto p: P)
			delete p;
		for(auto l: rmap)
			delete l;
	}
	
	ForwardState *copy() const {
		auto s = new ForwardState(man);
		for(auto p: P)
			s->P.add(new ForwardPredicate(*p));
		s->M = M;
		s->MM = MM;
		s->b = b;
		return s;
	}

	void update(const sem::inst& i) {
		//cerr << "Updating: " << i << io::endl;
		switch(i.op) {
		case BRANCH:
			b = true; break;
		case SETI:
			setReg(i.d(), man.makeCst(i.cst())); break;
		case SET:
			setReg(i.d(), getReg(i.a())); break;
		case NEG:
		case NOT:
			setReg(i.d(), man.makeOp(opcode(i.op), getReg(i.a()))); break;
		case ADD:
		case SUB:
		case SHL:
		case SHR:
		case ASR:
		case AND:
		case OR:
		case XOR:
		case MUL:
		case MULU:
		case DIV:
		case DIVU:
		case MOD:
		case MODU:
		case MULH:
		case CMP:
		case CMPU:
			setReg(i.d(), man.makeOp(opcode(i.op), getReg(i.a()), getReg(i.b()))); break;
		case LOAD:
			setReg(i.d(), man.makeMem(getReg(i.a()), i.type())); break;
		case STORE:
			setMem(i.type(), i.addr(), getReg(i.reg())); break;
		case SCRATCH:
			scratch(i.d()); break;
		case ASSUME:
			assume(i.sr(), i.cond()); break;
		case NOP:
		case TRAP:
		case STOP:
		case SETP:
		case SPEC:
		case FORK:
		case JOIN:
		case MEET:
			break;
		case IF:
			ASSERTP(false, "unsupported IF");
			break;
		}
	}
	
	void print(io::Output& out) {
		if(b)
			out << "[taken]\n";
		else
			out << "[not taken]\n";
		for(auto p: M.pairs()) {
			if(p.fst < 0)
				out << 't' << -p.fst;
			else
				out << 'r' << p.fst;
			out << " = " << p.snd << io::endl;
		}
		for(auto p: MM.pairs()) {
			out << '*' << p.fst << " = " << p.snd << io::endl;
		}
		for(auto p: P)
			out << p << io::endl;
	}

    inline bool isTakingBranch() const {return b;}
    inline const Vector<ForwardPredicate *>& predicates() const{return P;};
	
public:
	
	void genPreds(sem::Block& b) {
		
		// build the reverse map fo registers
		for(auto p: M.pairs())
			if(p.fst >= 0) {
				auto l = rmap.get(p.snd, nullptr);
				if(l == nullptr) {
					l = new List<int>();
					rmap.put(p.snd, l);
				}
				l->add(p.fst);
			}
		
		// build the reverse map for memory
		for(auto p: MM.pairs()) {
			auto l = mmap.get(p.snd, nullptr);
			if(l == nullptr) {
				l = new List<const Mem *>();
				mmap.put(p.snd, l);
			}
			l->add(p.fst);
		}
		
		// generate the predicates
		for(auto p: P) {
			lookup(p->arg1(), p->cond(), p->arg2(), b);
			lookup(p->arg2(), symetric(p->cond()), p->arg1(), b);
		}
	}
	
	
private:
	
	void lookup(const Expression *e1, sem::cond_t c, const Expression *e2, sem::Block& b) {
		
		// generate constraints for registers
		{
			auto ll = rmap.get(e1, nullptr);
			if(ll != nullptr) {
				auto g2 = forGen(e2);
				if(g2 != nullptr)
					for(auto r: *ll)
						genRegConstraint(r, c, e2, b);
			}			
		}
		
		// generate constrains for memory
		{
			auto ll = mmap.get(e1, nullptr);
			if(ll != nullptr) {
				auto g2 = forGen(e2);
				if(g2 != nullptr) {
					for(auto a: *ll) {
						auto g1 = forGen(a->address());
						if(g1 != nullptr)
							genMemConstraint(a->type(), g1, c, g2, b);
					}
				}
			}
		}
		
		// lookup below if any
		switch(e1->kind()) {
		case Expression::MON: {
				auto e = static_cast<const Monadic *>(e1);
				if(e->opcode() == sem::NEG)
					lookup(e->arg(), symetric(c), man.makeOp(sem::NEG, e2), b);
			}
			break;
		case Expression::BIN: {
				auto e = static_cast<const Dyadic *>(e1);
				switch(e->opcode()) {
				case sem::ADD:
					lookup(e->arg1(), c, man.makeOp(sem::ADD, e->arg2(), e2), b);
					lookup(e->arg2(), c, man.makeOp(sem::ADD, e->arg1(), e2), b);
					break;
				case sem::SUB:
					lookup(e->arg1(), c, man.makeOp(sem::ADD, e2, e->arg2()), b);
					lookup(e->arg2(), symetric(c), man.makeOp(sem::SUB, e->arg1(), e2), b);
					break;
				default:
					break;
				}
			}
			break;
		case Expression::MEM:  {
				auto m = static_cast<const Mem *>(e1);
				auto ea = forGen(m->address());
				if(ea != nullptr) {
					auto e = forGen(e2);
					if(e != nullptr)
						genMemConstraint(m->type(), ea, c, e, b);
				}
			}
			break;
		default:
			break;
		}
	}
	
	void genMemConstraint(sem::type_t t, const Expression *a, sem::cond_t c, const Expression *e, sem::Block& b) {
		const int addr = -1, val = -2, sr = -3;
		a->gen(b, addr);
		b.add(sem::load(val, addr, t));
		e->gen(b, sr);
		b.add(sem::cmp(sr, val, sr));
		b.add(sem::assume(c, sr));
		b.add(sem::store(val, addr, t));		
	}
	
	void genRegConstraint(int r, sem::cond_t c, const Expression *e, sem::Block& b) {
		const int x = -1, sr = -2;
		e->gen(b, x);
		b.add(sem::cmp(sr, r, x));
		b.add(sem::assume(c, sr));		
	}
	
	const Expression *getReg(int r) {
		auto *e = M.get(r, nullptr);
		if(e == nullptr)
			e = man.makeReg(r);
		M.put(r, e);
		return e;
	}
	
	const Expression *getMem(int r, sem::type_t t) {
		auto a = man.makeMem(getReg(t), t);
		auto e = MM.get(a, nullptr);
		if(e == nullptr)
			MM.put(a, a);
		return a;
	}
	
	void scratch(int r) {
		M.remove(r);
	}

	void setReg(int r, const Expression *e) {
		M.put(r, e);
	}

	void setMem(sem::type_t t, int a, const Expression *e) {

		// record the memory map
		auto ae = man.makeMem(getReg(a), t);
		MM.put(ae, e);
		
		// fix the other memory references
		Vector<const Mem *> must_eq, may_eq;
		auto aa = ae->makeAddress();
		for(auto k: MM.keys()) {
				auto ka = k->makeAddress();
				if(ka == aa)
					must_eq.add(k);
				else if(!(ka != aa))
					may_eq.add(k);
			}
		for(auto a: must_eq)
			MM.put(a, e);
		for(auto a: may_eq)
			MM.remove(a);
	}
	
	void assume(int r, sem::cond_t c) {
		auto *e = getReg(r);
		if(e == nullptr || e->kind() != Expression::BIN)
			return;
		auto ee = static_cast<const Dyadic *>(e);
		auto op = ee->opcode();
		if(op == sem::CMPU) {
			op = CMP;
			switch(c) {
			case sem::LT:	c = sem::ULT; break;
			case sem::LE:	c = sem::ULE; break;
			case sem::GT:	c = sem::UGT; break;
			case sem::GE:	c = sem::UGE; break;
			default: 		break;
			}
		}
		if(op == sem::CMP)
			P.add(new ForwardPredicate(ee->arg1(), c, ee->arg2()));
	}
	
	const Expression *forGen(const Expression *e) {
		
		// aliased register?
		{
			auto ll = rmap.get(e, nullptr);
			if(ll != nullptr) 
				return man.makeReg(ll->first());			
		}

		// aliased memory
		{
			auto ll = mmap.get(e, nullptr);
			if(ll != nullptr)
				for (auto m: *ll)
					if(forGen(m->address()) != nullptr)
						return m;
		}
		
		// look downward in sub-expressions
		switch(e->kind()) {
		case Expression::CST:
			return e;
		case Expression::MON: {
				auto oe = static_cast<const Monadic *>(e);
				auto e = forGen(oe->arg());
				if(e == nullptr)
					return nullptr;
				return man.makeOp(oe->opcode(), e);
			}
			break;
		case Expression::BIN: {
				auto oe = static_cast<const Dyadic *>(e);
				auto a1 = forGen(oe->arg1());
				if(a1 == nullptr)
					return nullptr;
				auto a2 = forGen(oe->arg2());
				if(a2 == nullptr)
					return nullptr;
				return man.makeOp(oe->opcode(), a1, a2);
			}
			break;
		default:
			return nullptr;
		}
	}
	
	Vector<ForwardPredicate *> P;
	ListMap<int, const Expression *> M;
	ListMap<const Mem *, const Expression *> MM;
	bool b;
	ListMap<const Expression *, List<int> *> rmap;
	ListMap<const Expression *, List<const Mem *> *> mmap;
	ExpressionManager& man;
};

inline io::Output& operator<<(io::Output& out, ForwardState *s) { s->print(out); return out; }


class ForwardPredicateBuilder: public BBProcessor, public FilterMaker {
public:
	static p::declare reg;
	ForwardPredicateBuilder(): BBProcessor(reg), _max(0) {
		EMPTY.add(sem::stop());
	}

	static sem::Block EMPTY;
	
	const sem::Block& takenFilter(otawa::Block *v) override {
		return *_map.get(v, pair_t(&EMPTY, &EMPTY)).snd;
	}
	
	const sem::Block& notTakenFilter(otawa::Block *v) override {
		return *_map.get(v, pair_t(&EMPTY, &EMPTY)).fst;
	}

	int maxTemp() override { return _max; }

protected:
	
	void destroy(WorkSpace *ws) override {
		for(const auto& p: _map) {
			if(p.fst != &EMPTY)
				delete p.fst;
			if(p.snd != &EMPTY)
				delete p.snd;
		}
	}
	
	void processBB(WorkSpace *ws, CFG *g, otawa::Block *v)  override {
		if(!v->isBasic())
			return;
		auto b = v->toBasic();

		// build the states
		Vector<ForwardState *> states;
		auto s = new ForwardState(man);
		states.add(s);
		sem::Block block;
		for(auto i: *b) {
			block.clear();
			i->semInsts(block);
			block.add(sem::stop());
			process(block, states);
		}
		if(logFor(LOG_BLOCK))
			for(auto s: states)
				log << s << io::endl;
		
		// generate the constraints
		sem::Block *bs[] = { nullptr, nullptr };
		int lf[] = { -1, -1 };
		for(auto s: states)
			if(!s->predicates().isEmpty()) {
				int i = s->isTakingBranch() ? 1 : 0;
				if(bs[i] == nullptr)
					bs[i] = new sem::Block();
				bs[i]->add(sem::nop());
				int l = bs[i]->length();
				s->genPreds(*bs[i]);
				if(bs[i]->length() == l)
					bs[i]->pop();
				else {
					(*bs[i])[l - 1] = sem::fork(bs[i]->length() - l);
					lf[i] = l - 1;
				}
			}
		
		// store the constraints
		for(int i = 0; i < 2; i++) {
			if(bs[i] == nullptr)
				bs[i] = &EMPTY;
			else {
				if(lf[i] >= 0)
					(*bs[i])[lf[i]] = sem::nop();
				bs[i]->add(sem::stop());
				updateMax(*bs[i]);
			}
		}
		_map.put(v, pair(bs[0], bs[1]));
		if(logFor(LOG_BLOCK)) {
			log << "[not-taken]\n" << *bs[0] << io::endl;
			log << "[taken]\n" << *bs[1] << io::endl;
		}
		
		// cleanup states
		for(auto s: states)
			delete s;
	}

	void process(const sem::Block& block, Vector<ForwardState *>& states) {
		Vector<Pair<t::uint32, ForwardState *> > stack;
		for(auto s: states)
			stack.add(pair(t::uint32(0), s));
		ForwardState *ns;
		while(!stack.isEmpty()) {
			auto c = stack.pop();
			for(t::uint32 pc = c.fst; block[pc].op != sem::STOP; pc++) {
				switch(block[pc].op) {
				case sem::NOP:
					break;
				case sem::IF:
					ns = c.snd->copy();
					states.add(ns);
					ns->update(sem::assume(sem::invert(block[pc].cond()), block[pc].sr()));
					stack.push(pair(pc + block[pc].jump() + 1, ns));
					c.snd->update(sem::assume(block[pc].cond(), block[pc].sr()));
					break;
				case FORK: 
					ns = c.snd->copy();
					states.add(ns);
					stack.push(pair(pc + block[pc].jump() + 1, ns));
					break;
				default:
					c.snd->update(block[pc]);
					break;
				}
			}
		}
	}
	
	void updateMax(sem::Block& b) {
		for(const auto& i: b)
			switch(i.op) {
				case NOP:
				case TRAP:
				case STOP:
				case FORK:
				case BRANCH:
					break;
				case IF:
					_max = max(_max, -i.sr());
					break;					
				case LOAD:
				case STORE:
					_max = max(_max, -i.reg());
					_max = max(_max, -i.addr());
					break;
				case SCRATCH:
				case SETI:
				case SETP:
				case ASSUME:
					_max = max(_max, -i.d());
					break;
				case SET:
				case NEG:
				case NOT:
					_max = max(_max, -i.d());
					_max = max(_max, -i.a());
					break;
				case CMP:
				case CMPU:
				case ADD:
				case SUB:
				case SHL:
				case SHR:
				case ASR:
				case AND:
				case OR:
				case XOR:
				case MUL:
				case MULU:
				case DIV:
				case DIVU:
				case MOD:
				case MODU:
				case MULH:
				case JOIN:
				case MEET:
					_max = max(_max, -i.d());
					_max = max(_max, -i.a());
					_max = max(_max, -i.b());
					break;				
			}
	}
	
	void dumpBB(otawa::Block *v, io::Output &out) override {
		if(!v->isBasic())
			return;
		auto p = _map.get(v);
		if(!p)
			return;
		if((*p).fst != &EMPTY) {
			out << "not-taken\n";
			for(int i = 0; i < (*p).fst->length(); i++)
				out << "\t" << (*(*p).fst)[i] << io::endl;
		}
		if((*p).snd != &EMPTY) {
			out << "not-taken\n";
			for(int i = 0; i < (*p).snd->length(); i++)
				out << "\t" << (*(*p).snd)[i] << io::endl;
		}
	}
	
private:
	typedef Pair<sem::Block *, sem::Block *> pair_t;
	HashMap<otawa::Block *, pair_t> _map;
	int _max;
	ExpressionManager man;
};


///
sem::Block ForwardPredicateBuilder::EMPTY;


/**
 */
p::declare ForwardPredicateBuilder::reg = p::init("otawa::clp::ForwardPredicateBuilder", Version(2, 0, 0))
	.extend<BBProcessor>()
	.provide(FILTER_FEATURE)
	.make<ForwardPredicateBuilder>();

/**
 * This feature ensure that filter, expressed as semantic instructions, have been
 * provided for each conditional basic block.
 * 
 * **Default implementation:** BackwardPredicateBuilder
 * @ingroup clp
 */
p::interfaced_feature<FilterMaker> FILTER_FEATURE("otawa::clp::FILTER_FEATURE", p::make<ForwardPredicateBuilder>());

}} // otawa::pred
