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

#include <otawa/clp/Domain.h>
#include <otawa/prog/Symbol.h>
#include <otawa/ipet/FlowFactLoader.h>

#define CLP_CHECK(c)	c
//#define CLP_CHECK(c)

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
private:
	Vector<context_t> s;
};


/**
 * Get the value of a register.
 * @param state	State to look in.
 * @param i		Register number.
 */
inline const Value& get(const State& state, int i) {
	return state.get(Value(REG, i));
}


/**
 * Set a register to the given value
 * @param state is the initial state
 * @param i is the regsiter identifier (i < 0 for temp registers)
 * @param v is the value to set
 * @return the new state
 */
inline const void set(clp::State& state, int i, const Value& v) {
	state.set(Value(clp::REG, i), v);
}



///
Domain::Domain(Process *proc):
	currentInst(0),
	bBuildFilters(false),
	_process(proc),
	istate(nullptr),
	currentAccessAddress(0),
	cs(nullptr),
	fs(nullptr),
	init(new State()),
	stack(new ContextStack())
{
	proc->semInit(b);
	cs = init;
	update(BOTH);
	OTAWA_CLP_CHECK(
		State::EMPTY.lock();
		State::FULL.lock();
	)
}


///
Domain::~Domain() {
	delete stack;
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
	init->set(Value(VAL, addr.offset()), v);
}


/**
 * Set a register to the given value
 * @param state is the initial state
 * @param i is the regsiter identifier (i < 0 for temp registers)
 * @param v is the value to set
 * @return the new state
 */
void Domain::set(clp::State& state, int i, const clp::Value& v) {
	clp::Value addr(clp::REG, i);
	return state.set(addr, v);
}


///
ai::State *Domain::bot() {
	return &State::EMPTY;
}


///
ai::State *Domain::top() {
	return &State::FULL;
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
	State *res;
	if(s1 == &State::FULL || s2 == &State::FULL)
		res = &State::FULL;
	else if(s1 == &State::EMPTY)
		res = s2;
	else if(s2 == &State::EMPTY)
		res = s1;
	else if(s1 == s2)
		res = s1;
	else {
		res = new State(*s1);
		res->join(*s2);
		res->lock();
	}
	
	CLP_CHECK(
		/*cerr << "s1        = "; s1->print(cerr); cerr << io::endl;
		cerr << "s2        = "; s2->print(cerr); cerr << io::endl;
		cerr << "J(s1, s2) = "; res->print(cerr); cerr << io::endl;
		cerr << io::endl;*/
		ASSERT(s1->subsetOf(*res));
		ASSERT(s2->subsetOf(*res));
	)
	return res;
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
		b.clear();
		i->semInsts(b);
		for(auto s: b) {
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
void Domain::doLoad(State& state, const sem::inst& i) {
	Value addrclp = get(state, i.a());
	Value val = Value::bot;
	
	// T address or too many values to load
	if(addrclp == Value::all || addrclp.mtimes() >= MEMORY_ACCESS_THRESHOLD) {
		val = Value::all;
	}

	// reading in normal memory
	else if(istate == nullptr || !istate->isReadOnly(Address(addrclp.start())))
		for(unsigned int m = 0; m <= addrclp.mtimes(); m++)
			val.join(state.get(Value(VAL, addrclp.lower() + addrclp.delta() * m)));
	
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
void Domain::doStore(State& state, const sem::inst& i) {
	Value addrclp = get(state, i.a());

	// store at T
	if(addrclp == Value::all) {
		Pair<Address, Address> accessRange = otawa::ACCESS_RANGE(currentInst);
		if(accessRange.fst != Address::null && accessRange.snd != Address::null)
			state.set(Value(VAL, accessRange.fst.offset(), 1, accessRange.snd.offset() -accessRange.fst.offset()), get(state, i.d()));
		else {
			state.set(addrclp, get(state, i.d()));
			if(!store_to_T.contains(currentInst))
				store_to_T.add(currentInst);
		}
	}

	// normal store
	else if(addrclp.mtimes() < MEMORY_ACCESS_THRESHOLD) {
		if(addrclp.mtimes() == 0)
			state.set(Value(addrclp.lower()), get(state, i.d()));
		else {
			Value x = get(state, i.d());
			for(unsigned int m = 0; m <= addrclp.mtimes(); m++) {
				auto addr = Value(VAL, addrclp.lower() + addrclp.delta() * m);
				auto val = state.get(addr); 
				val.join(x);
				state.set(addr, val);
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
 * @param select	Select which kind of path to look for.
 */
void Domain::update(branch_t select) {
	if(cs == &State::EMPTY)
		return;
	fs = &State::EMPTY;
	bool branch = false;
	
	// run on the instructions
	for(int pc = 0; true; pc++) {
		
		// end of path?
		if(pc >= b.length() || b[pc].op == sem::CONT) {
			if((branch && (select == BOTH || select == TAKEN))
			|| (!branch && (select == BOTH || select == NOT_TAKEN))) {
				if(fs == &State::EMPTY)
					fs = cs;
				else {
					fs->join(*cs);
					delete cs;
				}		
			}
			else
				delete cs;
			if(stack->isEmpty())
				break;
			else
				stack->pop(pc, branch, cs);
		}
		
		// perform the instruction
		const sem::inst& i = b[pc];
		switch(i.op) {
			
		case sem::IF:
		case sem::FORK:
			stack->push(pc + i.jump(), branch, new State(*cs));
			break;

		case sem::BRANCH:
		case sem::TRAP:
			branch = true;
			break;

		case sem::CONT:
			pc = b.length();
			break;

		case sem::NOP:
		case sem::ASSUME:
			break;
		
		case sem::LOAD:
			doLoad(*cs, i);
			break;
		case sem::STORE:
			doStore(*cs, i);
			break;

		case sem::SETP:
		case sem::CMP:
		case sem::CMPU:
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
	}

	cs = fs;
}


///
ai::State *Domain::update(Edge *e, ai::State *_s) {
	return _s;
}


ai::State *Domain::update(Block *v, ai::State *_s) {
	if(!v->isBasic())
		return _s;
	if(_s == &State::EMPTY)
		return _s;
	cs = new State(*static_cast<State *>(_s));
	auto bb = v->toBasic();

	// for loop header apply widening
	if(LOOP_HEADER(bb)) {
		State *ps = loop_states.get(bb, nullptr);
		if(ps == nullptr) {
			ps = new State(*cs);
			OTAWA_CLP_CHECK(ps->lock();)
			loop_states.put(bb, ps);
		}
		else {
			CLP_CHECK(State *ops = new State(*ps));
			OTAWA_CLP_CHECK(ps->unlock();)
			ps->widening(*cs, MAX_ITERATION(v));
			OTAWA_CLP_CHECK(ps->lock();)
			CLP_CHECK(
				ASSERT(cs->subsetOf(*ps));
				ASSERT(ops->subsetOf(*ps));
				delete ops;
			)
			cs->copy(*ps);
		}
	}
	
	// traverse all bundles
	for(auto bu: bb->bundles()) {
		b.clear();
		bu.semInsts(b);
		currentInst = bu.first();
		update(BOTH);
		if(cs == &State::EMPTY)
			break;
	}
	OTAWA_CLP_CHECK(cs->unlock();)
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
	b.clear();
	bu.semInsts(b);
	currentInst = bu.first();
	update(BOTH);
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
	b.clear();
	inst->semInsts(b);
	currentInst = inst;
	b[sem] = sem::cont();
	b.setLength(sem + 1);
	update(BOTH);
	return cs;	
}

}} 	// otawa::clp
