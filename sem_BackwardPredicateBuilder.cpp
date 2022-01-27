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

#include "otawa/pred/predicates.h"

#include <elm/data/HashMap.h>
#include <elm/data/ListMap.h>
#include <otawa/proc/BBProcessor.h>

namespace otawa { namespace pred {

using namespace sem;

class ExpressionComparator {
public:
	int doCompare(const Expression * e1, const Expression * e2) const { return e1->compare(e2); }
	static int compare(const Expression * e1, const Expression * e2) { return e1->compare(e2); }
};

class BackwardState {
public:
	
	BackwardState(pred::ExpressionManager& _man): br(false), man(_man) {}
    ~BackwardState(){
        for (auto x:regs)
            delete &(*x);
        for (auto& x:mem.pairs()) {
            delete x.fst;
            delete x.snd;
        }
    };

	BackwardState *copy() const {
		auto s = new BackwardState(man);
		for(const auto& r: regs.pairs())
			s->regs.put(r.fst, r.snd);
		for(const auto& m: mem.pairs())
			s->mem.put(m.fst, m.snd);
		for(const auto& c: sr.pairs())
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
			replace(i.d(), man.makeCst(i.cst()));
			break;
		case SET:
			replace(i.d(), man.makeReg(i.a()));
			extend(i.d(), i.a(), [](int r, cond_t c, const Expression * e)
				{ return Predicate::reg(r, c, e); });
			break;
		case NEG:
			replace(i.d(), man.makeOp(NEG, man.makeReg(i.a())));
			extend(i.d(), i.a(), [](int r, cond_t c, const Expression * e)
				{ return Predicate::reg(r, invert(c), e); });
			break;
		case NOT:
			replace(i.d(), man.makeOp(NOT, man.makeReg(i.a())));
			break;
		case ADD:
			replace(i.d(), man.makeOp(ADD,
				man.makeReg(i.a()), man.makeReg(i.b())));
			extend(i.d(), i.a(), [i, this](int r, cond_t c, const Expression * e)
				{ return Predicate::reg(r, c, man.makeOp(SUB, e, man.makeReg(i.b()))); });
			extend(i.d(), i.b(), [i, this](int r, cond_t c, const Expression * e)
				{ return Predicate::reg(r, c, man.makeOp(SUB, e, man.makeReg(i.a()))); });
			break;
		case SUB:
			replace(i.d(), man.makeOp(SUB,
				man.makeReg(i.a()), man.makeReg(i.b())));
			extend(i.d(), i.a(), [i, this](int r, cond_t c, const Expression * e)
				{ return Predicate::reg(r, c, man.makeOp(ADD, e, man.makeReg(i.b()))); });
			extend(i.d(), i.b(), [i, this](int r, cond_t c, const Expression * e)
				{ return Predicate::reg(r, invert(c), man.makeOp(SUB, man.makeReg(i.a()), e)); });
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
			replace(i.d(), man.makeOp(opcode(i.op),
				man.makeReg(i.a()), man.makeReg(i.b())));
			break;
		case LOAD:
			replace(i.d(), man.makeMem(man.makeReg(i.a()), i.type()));
			extend(i.d(), -1, [i, this](int r, cond_t c, const Expression * e)
				{ return Predicate::mem(man.makeReg(i.b()), i.type(), c, e); });
			break;
		case ASSUME:
			sr.put(i.sr(), i.cond());
			break;
		case CMP:
            if (br)
			    addPred(i.d(), i.a(), i.b(), false);
			break;
		case CMPU:
            if (br)
			    addPred(i.d(), i.a(), i.b(), true);
			break;
		case NOP:
		case STORE:
			replace(i.d(), man.makeMem(man.makeReg(i.a()), i.type()));
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
		for(const auto& r: regs.pairs()) {
            if (r.fst < 0 )
                out << "T" << -r.fst << "\' = " << r.snd << io::endl;
            else
                out << "R" << r.fst << "\' = " << r.snd << io::endl;
        }
		for(const auto& m: mem.pairs())
			out << "M[" << m.fst << "] = " << m.snd << io::endl;

        for(const auto& s: sr.pairs()){
            out << "R" << s.fst << " ~ " << s.snd << io::endl;
        }
		for(auto p: preds) {
            p->print(out);
            out << io::endl;
        }
	}

    inline bool isTakingBranch() const {return br;}
    inline const Vector<Predicate *>& predicates() const{return preds;};
    inline const ListMap<int, sem::cond_t>& sr_predicates() const{return sr;};

private:

    /**
     * remove from preds all predicate that defines register r
     * @param r the number of the register r
     */
    void clearPredDefined(int r){
        Vector<Predicate*> new_preds;
        for (auto* pred:preds)
            if (pred->definedReg() != r)
                new_preds.add(pred);

        preds = new_preds;
    }

    void replace(int r, const Expression * e) {
        for(auto& ep: regs)
            ep = ep->substitute(r, e);
        for(auto& ep: mem)
            ep = ep->substitute(r, e);
        for(auto p: preds)
            if (p->definesAnyReg() && !e->contains(p->definedReg()))
                p->substitute(r, e);
        if(regs.hasKey(r)) {
            //cerr << "Already define: " << r << io::endl;
        }
        else {
            regs.put(r, e);
            /*if (r >= 0)
                cerr << "Putting: R" << r << "': " << e << io::endl;
            else
                cerr << "Putting: T" << -r << "': " << e << io::endl;*/
        }
    }

    void addPred(int s, int a, int b, bool uns) {
		auto c = sr.get(s, NO_COND);
		if(c != NO_COND) {
			if(uns)
				switch(c) {
                // XXX: ZHEN: i don't know why we replace all signed comparison by unsigned ones
				case LT:	c = ULT; break;
				case LE:	c = ULE; break;
				case GE:	c = UGE; break;
				case GT:	c = UGT; break;
				default:	break;
				}
            if (c == sem::EQ || c == sem::NE){
                preds.add(Predicate::reg(a, c, man.makeReg(b)));
                preds.add(Predicate::reg(b, c, man.makeReg(a)));
            }
            else {
                preds.add(Predicate::reg(a, c, man.makeReg(b)));
                preds.add(Predicate::reg(b, sem::invert(c), man.makeReg(a)));
            }
		}
	}

    /**
     * From known predicates on a register, extend them to others
     * @param r
     * @param rp
     * @param f
     */
	void extend(int r, int rp, const std::function<Predicate *(int, cond_t, const Expression *)>& f) {
		Vector<Predicate *> added;
		for(auto p: preds)
			if(p->defines(r) && !p->contains(rp))
				added.add(f(rp, p->condition(), p->expression()));
		preds.addAll(added);
	}
	
	void store(int r, int a, sem::type_t t) {
		auto addr = man.makeMem(man.makeReg(a), t);
		if(!mem.hasKey(addr))
			mem.put(addr, man.makeReg(r));
	}
	
	ListMap<int, const Expression *> regs;
	ListMap<const Expression * , const Expression * , ExpressionComparator> mem;
	ListMap<int, sem::cond_t> sr;
	Vector<Predicate *> preds;
	bool br;
	ExpressionManager& man;
};

inline io::Output& operator<<(io::Output& out, BackwardState *s) { s->print(out); return out; }


/**
 * Interface to get filters to apply to a value analysis state to improve its
 * precision based on BB predicate building.
 * @ingroup pred
 */
/*class FilterMaker {
public:
	virtual const sem::Block& takenFilter(otawa::Block *v) = 0;
	virtual const sem::Block& notTakenFilter(otawa::Block *v) = 0;
	virtual int maxTemp() = 0;
};

extern p::interfaced_feature<FilterMaker> FILTER_FEATURE;
*/
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
	
	void setup(WorkSpace *ws) override {
		man = new ExpressionManager;
	}
	
	void destroy(WorkSpace *ws) override {
		for(const auto& p: _map) {
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
		Vector<BackwardState *> states;
		states.add(new BackwardState(*man));
		for(int i =  bundles.length() - 1; i >= 0; i--) {
			block.clear();
			bundles[i].semInsts(block);
            // XXX To fork, or not to fork, that is a question
            //block.fork();
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
        // first, separate from states, those are taken and those are not taken
        Vector<BackwardState*> taken;
        Vector<BackwardState*> not_taken;
        for (auto* s: states)
            if (s->isTakingBranch())
                taken.add(s);
            else
                not_taken.add(s);

        // Generate semantic instructions
        sem::Block gen_block;
        // First, generate the two cases: taken and not taken
        // the address of the end of the body is not known now,
        // so it has to be backpatched later.
        gen_block.add(sem::fork(-1));

        // taken case
        if (taken.count() > 1) {
            for (auto *s: taken) {
                // same problem. the destination address is not known, tobe patched later.
                gen_block.add(sem::fork(-1));
                // record the address of the fork, to easy the backpatch later
                int fork_addr = gen_block.count() - 1;
                // for each predicate in the state (of that path)
                for (const auto &pred: s->predicates()) {
                    // generate the sem insts if the pred doesn't define a temp register
                    // XXX ZHEN: i don't think this would badly impact the precision
                    if (pred->definedReg()>=0)
                        pred->gen(gen_block);
                }
                // backpatch the fork: nb of generated insts = current count - (fork_addr + 1)
                gen_block[fork_addr] = sem::fork(gen_block.count() - fork_addr - 1);
            }
        }
        else{ //taken has only one state
            if (!taken.isEmpty())
                for (const auto &pred: taken[0]->predicates())
                    if (pred->definedReg()>=0)
                        pred->gen(gen_block);
        }

        // the end of the first branch
        gen_block.add(sem::stop());
        // backpatch the address of the fork at the beginning.
        gen_block[0] = sem::fork(gen_block.count()-1);

        // not taken case, same algo of the taken case
        if ( not_taken.count() > 1) {
            for (auto *s: not_taken) {
                gen_block.add(sem::fork(-1));
                int fork_addr = gen_block.count() - 1;
                for (const auto &pred: s->predicates())
                    if (pred->definedReg()>=0)
                        pred->gen(gen_block);
                gen_block[fork_addr] = sem::fork(gen_block.count() - fork_addr - 1);
            }
        }
        else{ // not_taken has only one state
            if (!not_taken.isEmpty())
                for (const auto &pred: not_taken[0]->predicates())
                    if (pred->definedReg()>=0)
                        pred->gen(gen_block);
        }
        gen_block.add(sem::stop());

        // DEBUG: print the generated instructions
        cerr << "===== GEN =====" << io::endl;
        for (const auto& inst: gen_block)
            cerr << inst << io::endl;
        cerr << "===== END GEN =====" << io::endl;
	}

	void process(const sem::Block& block, Vector<BackwardState *>& states) {
		sem::Block cur;
		Vector<BackwardState *> init_states = states;
		Vector<BackwardState *> cur_states;
		
		// traverse all possibles paths
        // this pair is used to record (a) the address in the bundle
        // (b) and the address of the last inst before deviation.
        // Upon deviation (due to IF or FORK), it takes one path, store the addresses into the stack
        // then, next iteration, takes another path
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
				pc = pc + block[pc].jump() + 1;
			}
		}
	
	}
	
private:
	typedef Pair<sem::Block *, sem::Block *> pair_t;
	HashMap<otawa::Block *, pair_t> _map;
	int _max;
	ExpressionManager *man;
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
//p::interfaced_feature<FilterMaker> FILTER_FEATURE("otawa::clp::FILTER_FEATURE", p::make<BackwardPredicateBuilder>());

}} // otawa::pred
