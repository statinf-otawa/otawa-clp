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
#include <otawa/proc/BBProcessor.h>

namespace otawa { namespace pred {

using namespace sem;

class ExpressionComparator {
public:
	int doCompare(Expression *e1, Expression *e2) const { return e1->compare(e2); }
	static int compare(Expression *e1, Expression *e2) { return e1->compare(e2); }
};

class State {
public:
	
	State(): br(false) {}

	State *copy() const {
		auto s = new State();
		for(auto r: regs.pairs())
			s->regs.put(r.fst, r.snd);
		for(auto m: mem.pairs())
			s->mem.put(m.fst->copy(), m.snd->copy());
		for(auto c: sr.pairs())
			s->sr.put(c.fst, c.snd);
		for(auto p: preds)
			s->preds.add(p->copy());
		return s;
	}

	void update(const sem::inst& i) {
		cerr << "Updating: " << i << io::endl;
		switch(i.op) {
		case BRANCH:
			br = true;
			break;
		case SETI:
			replace(i.d(), Expression::cst(i.cst()));
			break;
		case SET:
			replace(i.d(), Expression::reg(i.a()));
			extend(i.d(), i.a(), [](int r, cond_t c, Expression *e)
				{ return Predicate::reg(r, c, e->copy()); });
			break;
		case NEG:
			replace(i.d(), Expression::op(NEG, Expression::reg(i.a())));
			extend(i.d(), i.a(), [](int r, cond_t c, Expression *e)
				{ return Predicate::reg(r, invert(c), e->copy()); });
			break;
		case NOT:
			replace(i.d(), Expression::op(NOT, Expression::reg(i.a())));
			break;
		case ADD:
			replace(i.d(), Expression::op(ADD,
				Expression::reg(i.a()), Expression::reg(i.b())));
			extend(i.d(), i.a(), [i](int r, cond_t c, Expression *e)
				{ return Predicate::reg(r, c, Expression::op(SUB, e->copy(), Expression::reg(i.b()))); });
			extend(i.d(), i.b(), [i](int r, cond_t c, Expression *e)
				{ return Predicate::reg(r, c, Expression::op(SUB, e->copy(), Expression::reg(i.a()))); });
			break;
		case SUB:
			replace(i.d(), Expression::op(SUB,
				Expression::reg(i.a()), Expression::reg(i.b())));
			extend(i.d(), i.a(), [i](int r, cond_t c, Expression *e)
				{ return Predicate::reg(r, c, Expression::op(ADD, e->copy(), Expression::reg(i.b()))); });
			extend(i.d(), i.b(), [i](int r, cond_t c, Expression *e)
				{ return Predicate::reg(r, invert(c), Expression::op(SUB, Expression::reg(i.a()), e->copy())); });
			break;
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
			replace(i.d(), Expression::op(opcode(i.op),
				Expression::reg(i.a()), Expression::reg(i.b())));
			break;
		case LOAD:
			replace(i.d(), Expression::mem(Expression::reg(i.a()), i.type()));
			extend(i.d(), -1, [i](int r, cond_t c, Expression *e)
				{ return Predicate::mem(Expression::reg(i.b()), i.type(), c, e->copy()); });
			break;
		case ASSUME:
			sr.put(i.d(), i.cond());
			break;
		case CMP:
			addPred(i.d(), i.a(), i.b(), false);
			break;
		case CMPU:
			addPred(i.d(), i.a(), i.b(), true);
			break;
		case NOP:
		case STORE:
			replace(i.d(), Expression::mem(Expression::reg(i.a()), i.type()));
			store(i.d(), i.a(), i.type());
			break;
		case TRAP:
		case STOP:
		case SCRATCH:
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
		if(br)
			out << "[taken]\n";
		else
			out << "[not taken]\n";
		for(auto r: regs.pairs())
			out << "R" << r.fst << " = " << r.snd << io::endl;
		for(auto m: mem.pairs())
			out << "M[" << m.fst << "] = " << m.snd << io::endl;
		for(auto p: preds)
			out << p << io::endl;
	}
	
private:
	
	void replace(int r, Expression *e) {
		for(auto ep: regs)
			ep->substitute(r, e);
		for(auto ep: mem)
			ep->substitute(r, e);
		for(auto p: preds)
			if(!p->expression()->contains(r) && !e->contains(p->definedReg()))
				p->substitute(r, e);
		if(regs.hasKey(r)) {
			delete e;
			cerr << "Already define: " << r << io::endl;
		}
		else {
			regs.put(r, e);
			cerr << "Putting: " << r << ": " << e << io::endl;
		}
	}
	
	void addPred(int s, int a, int b, bool uns) {
		auto c = sr.get(s, NO_COND);
		if(c != NO_COND) {
			if(uns)
				switch(c) {
				case LT:	c = ULT; break;
				case LE:	c = ULE; break;
				case GE:	c = UGE; break;
				case GT:	c = UGT; break;
				default:	break;
				}
			preds.add(Predicate::reg(a, c, Expression::reg(b)));
			preds.add(Predicate::reg(b, sem::invert(c), Expression::reg(a)));
		}
	}
	
	void extend(int r, int rp, std::function<Predicate *(int, cond_t, Expression *)> f) {
		Vector<Predicate *> added;
		for(auto p: preds)
			if(p->defines(r) && !p->contains(rp))
				added.add(f(rp, p->condition(), p->expression()->copy()));
		preds.addAll(added);
	}
	
	void store(int r, int a, sem::type_t t) {
		auto addr = Expression::mem(Expression::reg(a), t);
		if(!mem.hasKey(addr))
			mem.put(addr, Expression::reg(r));
	}
	
	ListMap<int, Expression *> regs;
	ListMap<Expression *, Expression *, ExpressionComparator> mem;
	ListMap<int, sem::cond_t> sr;
	Vector<Predicate *> preds;
	bool br;
};

inline io::Output& operator<<(io::Output& out, State *s) { s->print(out); return out; }


/**
 * Interface to get filters to apply to a value analysis state to improve its
 * precision based on BB predicate building.
 * @ingroup pred
 */
class FilterMaker {
public:
	virtual const sem::Block& takenFilter(otawa::Block *v) = 0;
	virtual const sem::Block& notTakenFilter(otawa::Block *v) = 0;
	virtual int maxTemp() = 0;
};

extern p::interfaced_feature<FilterMaker> FILTER_FEATURE;

class BackwardPredicateBuilder: public BBProcessor, public FilterMaker {
public:
	static p::declare reg;
	BackwardPredicateBuilder(): BBProcessor(reg), _max(0) { }

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
		for(auto p: _map) {
			delete p.fst;
			delete p.snd;
		}
	}
	
	void processBB(WorkSpace *ws, CFG *g, otawa::Block *v)  override {
		if(!v->isBasic())
			return;
		auto b = v->toBasic();
		
		// get the bundles (for reverse traversal)
		Vector<BasicBlock::Bundle> bundles;
		for(auto bundle: b->bundles())
			bundles.add(bundle);
		
		// build the states
		sem::Block block;
		Vector<State *> states;
		states.add(new State());
		for(int i =  bundles.length() - 1; i >= 0; i--) {
			block.clear();
			bundles[i].semInsts(block);
			cerr << "DEBUG: bundle @" << bundles[i].address() << ":\n" << block << io::endl;
			process(block, states);
			cerr << "STATES:\n";
			for(auto s: states)
				cerr << s << io::endl;
			
		}
		cout << v << io::endl;
		for(auto s: states)
			cout << s << io::endl;
		
		// convert the states to semantic instructions
	}
	
	void process(const sem::Block& block, Vector<State *>& states) {
		sem::Block cur;
		Vector<State *> init_states = states;
		Vector<State *> cur_states;
		
		// traverse all possibles paths
		Vector<Pair<int, int> > stack;	// (PC+jump in block, PC in cur)
		//stack.push(pair(0, 0));
		int pc = 0;
		while(true) {
			
			// build a path
			for(; pc < block.length() && block[pc].op != sem::STOP; pc++)
				switch(block[pc].op) {
				case sem::NOP:
					break;
				case sem::IF:
					stack.push(pair(pc, cur.length()));
					cur.add(sem::assume(sem::invert(block[pc].cond()), block[pc].sr()));
					break;
				case FORK:
					stack.push(pair(pc, cur.length()));
					break;
				default:
					cur.add(block[pc]);
					break;
				}
			cur.add(sem::stop());
			
			// prepare the states
			cur_states.clear();
			if(!stack)
				cur_states = init_states;
			else
				for(auto s: init_states) {
					auto ns = s->copy();
					cur_states.add(ns);
					states.add(ns);
				}
		
			// execute the path backward
			for(auto s: cur_states)
				for(int i = cur.length() - 1; i >= 0; i--)
					s->update(cur[i]);

			// next path
			if(!stack)
				break;
			else {
				auto c = stack.pop();
				pc = c.fst;
				cur.setLength(c.snd);
				if(block[pc].op == sem::IF)
					cur.add(sem::assume(block[pc].cond(), block[pc].sr()));
				pc = pc + block[pc].jump();
			}
		}
	
	}
	
private:
	typedef Pair<sem::Block *, sem::Block *> pair_t;
	HashMap<otawa::Block *, pair_t> _map;
	int _max;
};


///
sem::Block BackwardPredicateBuilder::EMPTY;


/**
 */
p::declare BackwardPredicateBuilder::reg = p::init("", Version(2, 0, 0))
	.extend<BBProcessor>()
	.provide(FILTER_FEATURE)
	.make<BackwardPredicateBuilder>();


/**
 * This feature ensure that filter, expressed as semantic instructions, have been
 * provided for each conditional basic block.
 * 
 * **Default implementation:** BackwardPredicateBuilder
 * @ingroup clp
 */
p::interfaced_feature<FilterMaker> FILTER_FEATURE("otawa::clp::FILTER_FEATURE", p::make<BackwardPredicateBuilder>());

}} // otawa::pred
