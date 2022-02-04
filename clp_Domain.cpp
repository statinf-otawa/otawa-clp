/*
 *	clp::Domain class interface
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

#include <otawa/prog/Symbol.h>
#include <otawa/ipet/FlowFactLoader.h>

#include "otawa/clp/Domain.h"

#ifdef NDEBUG
#	define CLP_CHECK(c)
#	define DEBUG_CFG(c)
#else
#	define CLP_CHECK(c)		// c
#	define DEBUG_CFG(c)		// c
#endif


namespace otawa { namespace clp {

// TODO make them as parameters of the domain
#define MEMORY_ACCESS_THRESHOLD 1024
#define ARITHMETIC_THRESHOLD 1024
	
class ContextStack {
	typedef struct context_t {
		inline context_t() {}
		inline context_t(int p, bool b, State *c): pc(p), branch(b), cs(c) {}
		int pc;
		bool branch;
		State *cs;
	} context_t;
public:
	inline bool isEmpty() const { return s.isEmpty(); }
	inline void push(int p, bool b, State *c) { s.push(context_t(p, b, c)); }
	inline void pop(int& p, bool& b, State*& c) {
		p = s.top().pc;
		b = s.top().branch;
		c = s.top().cs;
		s.pop();
	}
	inline void collect(AbstractGC& gc)
		{ for(auto& c: s) gc.mark(c.cs, sizeof(State)); }
private:
	Vector<context_t> s;
};


/**
 * Get the value of a register.
 * @param state	State to look in.
 * @param i		Register number.
 */
inline const Value& get(const State& state, int i) {
	return state.getReg(i);
}


/**
 * Set a register to the given value
 * @param state is the initial state
 * @param i is the regsiter identifier (i < 0 for temp registers)
 * @param v is the value to set
 * @return the new state
 */
inline const void set(clp::State& state, int i, const Value& v) {
	state.setReg(i, v);
}



///
Domain::Domain(Process *proc, pred::FilterInfo *f, ListGC& gc_):
	_max(max(proc->maxTemp(), f == nullptr ? 0 : f->maxTemp())),
	currentInst(0),
	bBuildFilters(false),
	_process(proc),
	istate(nullptr),
	currentAccessAddress(0),
	cs(nullptr),
	fs(nullptr),
	init(new(gc_.alloc<State>()) State(_max)),
	ts(nullptr),
	tops(new(gc_.alloc<State>()) State(_max, Value::none)),
	bots(new(gc_.alloc<State>()) State(_max, Value::none)),
	stack(new ContextStack()),
	filter(f),
	gc(gc_)
{
	proc->semInit(buf);
	cs = init;
	update(buf, BOTH);
}


///
Domain::~Domain() {
	delete stack;
}


/**
 * Called to collect living states in the GC.
 * @param gc	GC to work with.
 */
void Domain::collect(AbstractGC& gc) {
	gc.mark(cs, sizeof(State));
	gc.mark(fs, sizeof(State));
	gc.mark(init, sizeof(State));
	gc.mark(ts, sizeof(State));
	gc.mark(tops, sizeof(State));
	gc.mark(bots, sizeof(State));
	stack->collect(gc);
	for(auto s: loop_states)
		gc.mark(s, sizeof(State));
}


/**
 *  Initialize a register in the init state from an address.
 *  @param reg		Register to initialize.
 *  @param address	Address to set in register.
 */
void Domain::initialize(const hard::Register *reg, const Address& address) {
	Value v;
	v = Value(VAL, address.offset());
	set(*init, reg->platformNumber(), v);
}

/**
 * Initialize a register from a DFA value.
 * @param reg	Register to initialize.
 * @param val	DFA value.
 */
void Domain::initialize(const hard::Register *reg, const dfa::Value val) {
	Value v(VAL, val.base(), val.delta(), val.count());
	set(*init, reg->platformNumber(), v);
}

/**
 * Initialize a memory cell from a DFA value.
 */
void Domain::initialize(Address addr, const dfa::Value val) {
	Value v(VAL, val.base(), val.delta(), val.count());
	init->store(Value(VAL, addr.offset()), v);
}


/**
 * Set a register to the given value
 * @param state is the initial state
 * @param i is the regsiter identifier (i < 0 for temp registers)
 * @param v is the value to set
 * @return the new state
 */
void Domain::set(clp::State& state, int i, const clp::Value& v) {
	return state.setReg(i, v);
}


///
ai::State *Domain::bot() {
	return bots;
}


///
ai::State *Domain::top() {
	return tops;
}


///
ai::State *Domain::entry() {
	return init;
}


///
bool Domain::equals(ai::State *_s1, ai::State *_s2) {
	auto s1 = static_cast<State *>(_s1);
	auto s2 = static_cast<State *>(_s2);
	return s1->equals(*s2);
}


///
ai::State *Domain::join(ai::State *_s1, ai::State *_s2) {
	auto s1 = static_cast<State *>(_s1);
	auto s2 = static_cast<State *>(_s2);
	if(s1 == tops || s2 == tops)
		ts = tops;
	else if(s1 == bots)
		ts = s2;
	else if(s2 == bots)
		ts = s1;
	else if(s1 == s2)
		ts = s1;
	else {
		ts = new(gc.alloc<State>()) State(*s1);
		ts->join(*s2);
	}
	return ts;
}


///
bool Domain::implementsPrinting() {
	return true;
}


///
void Domain::print(ai::State *_s, io::Output& out) {
	auto s = static_cast<State *>(_s);
	s->print(out, _process->platform());
}


///
bool Domain::implementsCodePrinting() {
	return true;
}


///
void Domain::printCode(Block *v, io::Output& out) {
	if(!v->isBasic())
		return;
	sem::Printer p( _process->platform());
	for(auto i: *v->toBasic()) {
		out << i->address() << ' ' << i << io::endl;
		buf.clear();
		i->semInsts(buf);
		for(auto s: buf) {
			out << '\t';
			p.print(out, s);
			out << io::endl;
		}
	}
}


///
void Domain::printCode(Edge *e, io::Output& out) {
}


///
bool Domain::implementsTracing() {
	return true;
}

///
void Domain::printTrace(ai::State *s, io::StructuredOutput& out) {
	static_cast<State *>(s)->print(out, _process->platform());
}


/**
 * Perform a load operation.
 */
void Domain::load(State& state, const sem::inst& i) {
	Value addrclp = get(state, i.a());
	Value val = Value::bot;
	
	// T address or too many values to load
	if(addrclp == Value::all || addrclp.mtimes() >= MEMORY_ACCESS_THRESHOLD) {
		val = Value::all;
	}

	// reading in normal memory
	else if(istate == nullptr || !istate->isReadOnly(Address(addrclp.start())))
		for(unsigned int m = 0; m <= addrclp.mtimes(); m++)
			val.join(state.load(Value(VAL, addrclp.lower() + addrclp.delta() * m)));
	
	// reading from constant memory
	else {
		if(!istate->isReadOnly(addrclp.stop()))
			val = Value::top;
		else
			for(unsigned int m = 0; m <= addrclp.mtimes(); m++) {
				auto a = addrclp.lower() + addrclp.delta() * m;
				if(!istate->isReadOnly(Address(a))) {
					val = Value::top;
					break;
				}
				switch(i.type()) {
				case sem::INT8:
					{ t::int8 x; _process->get(a, x); val.join(Value(VAL, x)); } break;
				case sem::UINT8:
					{ t::uint8 x; _process->get(a, x); val.join(Value(x)); } break;
				case sem::INT16:
					{ t::int16 x; _process->get(a, x); val.join(Value(x)); } break;
				case sem::UINT16:
					{ t::uint16 x; _process->get(a, x); val.join(Value(x)); } break;
				case sem::INT32:
					{ t::int32 x; _process->get(a, x); val.join(Value(x)); } break;
				case sem::UINT32:
					{ t::uint32 x; _process->get(a, x); val.join(Value(VAL, x)); } break;
				case sem::INT64:
				case sem::UINT64:
				case sem::FLOAT32:
				case sem::FLOAT64:
					val = Value::top;
					m = addrclp.mtimes();
					break;
				default:
					ASSERTP(false, "unsupported load type");
					break;
				}
			}
	}
	set(state, i.d(), val);
}


///
void Domain::store(State& state, const sem::inst& i) {
	Value addrclp = get(state, i.a());

	// store at T
	if(addrclp == Value::all) {
		Pair<Address, Address> accessRange = otawa::ACCESS_RANGE(currentInst);
		if(accessRange.fst != Address::null && accessRange.snd != Address::null)
			state.store(Value(VAL, accessRange.fst.offset(), 1, accessRange.snd.offset() -accessRange.fst.offset()), get(state, i.d()));
		else {
			state.store(addrclp, get(state, i.d()));
			if(!store_to_T.contains(currentInst))
				store_to_T.add(currentInst);
		}
	}

	// normal store
	else if(addrclp.mtimes() < MEMORY_ACCESS_THRESHOLD) {
		if(addrclp.mtimes() == 0)
			state.store(Value(addrclp.lower()), get(state, i.d()));
		else {
			Value x = get(state, i.d());
			for(unsigned int m = 0; m <= addrclp.mtimes(); m++) {
				auto addr = Value(VAL, addrclp.lower() + addrclp.delta() * m);
				auto val = state.load(addr); 
				val.join(x);
				state.store(addr, val);
			}
		}
	}
		
	// store all on the area (too many addresses)
	else {
		if(addrclp.mtimes() < UMAXn)
			state.clear(addrclp.start(), elm::abs(addrclp.delta()) * addrclp.mtimes());
		else {
			// TODO seems to be generally unsafe but maybe safe for normal C programs
			Symbol *sym = this->_process->findSymbolAt(addrclp.lower());
			if(sym == nullptr)
				// TODO alarm store to T
				state.clear(0, uintn_t(-1));
			else
				// TODO alarm store using symbol
				state.clear(sym->address().offset(), sym->size());
		}
	}
}


/**
 * Update the current state with the given sequence of semantic instruction.
 * @param b			Semantic instruction buffer to execute.
 * @param select	Select which kind of path to look for.
 */
void Domain::update(const sem::Block& b, branch_t select) {
	if(cs == bots)
		return;
	fs = cs;
	bool branch = false;
	
	// run on the instructions
	for(int pc = 0; true; pc++) {
		
		// end of path?
		if(pc >= b.length() || b[pc].op == sem::CONT) {
			if((branch && (select == BOTH || select == TAKEN))
			|| (!branch && (select == BOTH || select == NOT_TAKEN))) {
				if(fs != cs)
					fs->join(*cs);
			}
			else if(fs == cs)
				fs->copy(*bots);
			if(fs != cs)
				delete cs;	// intermediate state not managed by GC
			if(stack->isEmpty())
				break;
			else {
				stack->pop(pc, branch, cs);
				if(pc >= b.length())
					continue;
			}
		}

		// perform the instruction
		const sem::inst& i = b[pc];
		switch(i.op) {

		case sem::CONT:
			pc = b.length();
			break;

		case sem::BRANCH:
		case sem::TRAP:
			branch = true;
			break;

		case sem::CMP:
			set(*cs, i.d(), Value::compare(i.a(), i.b()));
			break;
			
		case sem::CMPU:
			set(*cs, i.d(), Value::compare(i.a(), i.b(), true));
			break;

		case sem::ASSUME:
			doAssume(*cs, i);
			break;
		
		case sem::FORK:
			ts = new State(*cs);	// intermediate state not managed by GC
			stack->push(pc + i.jump(), branch, ts);
			break;
			
		case sem::IF:
			ts = new State(*cs);	// intermediate state not managed by GC
			doAssume(*ts, sem::assume(sem::invert(i.cond()), i.sr()));
			stack->push(pc + i.jump(), branch, ts);
			doAssume(*cs, sem::assume(i.cond(), i.sr()));
			break;

		case sem::NOP:
			break;
			
		case sem::LOAD:
			load(*cs, i);
			break;
		case sem::STORE:
			store(*cs, i);
			break;

		case sem::SETP:
		case sem::SCRATCH:
			set(*cs, i.d(), Value::all);
			break;
			
		case sem::SET:
			set(*cs, i.d(), get(*cs, i.a()));
			break;
		case sem::SETI:
			set(*cs, i.d(), Value(VAL, i.cst()));
			break;

		case sem::ADD: {
				auto v = get(*cs, i.a());
				v.add(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::SUB: {
				auto v = get(*cs, i.a());
				v.sub(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::SHL: {
				auto v = get(*cs, i.a());
				v.shl(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::SHR: {
				auto v = get(*cs, i.a());
				v.shr(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::ASR: {
				auto v = get(*cs, i.a());
				v.asr(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::OR: {
				auto v = get(*cs, i.a());
				v._or(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::AND: {
				auto v = get(*cs, i.a());
				v._and(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::MUL: {
				auto v = get(*cs, i.a());
				v.mul(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::DIV: {
				auto v = get(*cs, i.a());
				v.div(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;
		case sem::MOD: {
				auto v = get(*cs, i.a());
				v.mod(get(*cs, i.b()));
				set(*cs, i.d(), v);
			}
			break;

		default:
			set(*cs, i.d(), Value::all);
			break;
		}
		
		// reduce to _|_?
		if(cs->isNone())
			pc = b.length();
	}

	cs = fs;
}


/**
 * Implements the ASSUME instruction.
 * @param s		State to update.
 * @param i		Assume instruction.
 */
void Domain::doAssume(State& s, const sem::inst& i) {
	auto x = get(s, i.sr());
	
	// get the compared values
	if(!x.isComp())
		return;
	auto r1 = x.r1();
	auto r2 = x.r2();
	auto x1 = get(s, r1);
	auto x2 = get(s, r2);
	
	// fix unsigned if required
	auto c = i.cond();
	if(x.uns())
		c = sem::unsignedCond(c);
		 
	// perform the comparison
	switch(i.cond()) {
	case sem::EQ:	x1.eq(x2); x2.eq(x1); break;
	case sem::NE:	x1.ne(x2); x2.ne(x1); break;
	case sem::LT:	x1.lt(x2); x2.ge(x1); break;
	case sem::LE:	x1.le(x2); x2.gt(x1); break;
	case sem::GT:	x1.gt(x2); x2.le(x1); break;
	case sem::GE:	x1.ge(x2); x2.lt(x1); break;
	case sem::ULT:	x1.ltu(x2); x2.geu(x1); break;
	case sem::ULE:	x1.leu(x2); x2.gtu(x1); break;
	case sem::UGT:	x1.gtu(x2); x2.leu(x1); break;
	case sem::UGE:	x1.geu(x2); x2.ltu(x1); break;
	default:		break;
	}
	
	// store the result
	set(s, r1, x1);
	set(s, r2, x2);
}


///
ai::State *Domain::update(Edge *e, ai::State *_s) {
	if(filter == nullptr || _s == bots)
		return _s;
	cs = new(gc.alloc<State>()) State(*static_cast<State *>(_s));
	//cs = static_cast<State *>(_s);
	const sem::Block *b;
	if(e->isNotTaken())
		b = &filter->notTakenFilter(e->source());
	else
		b = &filter->takenFilter(e->source());
	//cerr << "\nDEBUG: " << e << io::endl;
	//b->print(cerr);
	//cerr << "DEBUG: before: "; cs->print(cerr); cerr << io::endl;
	this->update(*b, BOTH);
	//cerr << "DEBUG: after: "; cs->print(cerr); cerr << io::endl;
	return cs;
}

///
ai::State *Domain::update(Block *v, ai::State *_s) {
	if(v->isEntry() || v->isExit())
		return _s;
	if(_s == bots)
		return _s;
	cs = new(gc.alloc<State>()) State(*static_cast<State *>(_s));
	//auto bb = v->toBasic();

	// for loop header apply widening
	if(LOOP_HEADER(v)) {
		ts = loop_states.get(v, nullptr);
		if(ts == nullptr) {
			ts = new(gc.alloc<State>()) State(*cs);
			loop_states.put(v, ts);
			DEBUG_CFG(cerr << "\t\twidening with _|_: "; print(ts, cerr); cerr << io::endl);
		}
		else {
			DEBUG_CFG(cerr << "\t\tbefore widening: "; print(ts, cerr); cerr << io::endl);
			int n = MAX_ITERATION(v);
			if(n == 0)
				return bots;
			else if(n >= 0)
				ts->widening(*cs, n);
			else {
				CLP_CHECK(if(n >= 0) fs = new State(*ts));
				ts->widening(*cs, n);
				//CLP_CHECK(cerr << "\n\n\n\nDEBUG: ts="; ts->print(cerr); cerr << io::endl);
				//CLP_CHECK(cerr << "\nDEBUG: cs="; cs->print(cerr); cerr << io::endl);
				//CLP_CHECK(cerr << "\nDEBUG: ts="; ts->print(cerr); cerr << io::endl);
				CLP_CHECK(if(n >= 0) ASSERT(cs->subsetOf(*ts)));
				CLP_CHECK(if(n >= 0) ASSERT(fs->subsetOf(*ts)));
				CLP_CHECK(if(n >= 0) delete fs);				
			}
			DEBUG_CFG(cerr << "\t\tafter widening: "; print(ts, cerr); cerr << io::endl);
			cs->copy(*ts);
		}
	}

	// traverse all bundles
	if(v->isBasic())
		for(auto bu: v->toBasic()->bundles()) {
			buf.clear();
			bu.semInsts(buf);
			currentInst = bu.first();
			update(buf, BOTH);
			if(cs == bots)
				break;
		}
	//cerr << "DEBUG: after: "; cs->print(cerr); cerr << io::endl;
	return cs;
}

/**
 * Get the state after the execution of the given bundle.
 * @param bu			Bundle to execute.
 * @param s			Input state.
 * @param select	Selection of issue (taken, not taken, both).
 * @return			Output state (possibly input state updated).
 */
State *Domain::update(const BaseBundle<BasicBlock::InstIter>& bu, State *s, branch_t select) {
	cs = s;
	buf.clear();
	bu.semInsts(buf);
	currentInst = bu.first();
	update(buf, BOTH);
	return cs;
}

/**
 * Build the state before the execution of sem semantic instruction inside
 * the given machine instruction.
 * @param inst		Current machine instruction.
 * @param sem		Targetted semantic instruction.
 * @param s			Input state (possibly modified).
 * @param select	Branch selection.
 * @return			State just before the semantic instruction.
 */
State *Domain::update(Inst *inst, int sem, State *s, branch_t select) {
	cs = s;
	buf.clear();
	inst->semInsts(buf);
	currentInst = inst;
	buf[sem] = sem::cont();
	buf.setLength(sem + 1);
	update(buf, BOTH);
	return cs;	
}

} } 	// otawa::clp
