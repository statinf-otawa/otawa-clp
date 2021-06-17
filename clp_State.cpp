/*
 *	clp::State class implementation
 *	
 *	This file is part of OTAWA
 *	Copyright (c) 2011, IRIT UPS.
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

#include <otawa/clp/State.h>
#include <otawa/hard/Platform.h>

namespace otawa { namespace clp {
	
/**
 * @class State
 * state for the @ref clp analysis.
 * @ingroup clp
 */

/**
 * Constructors of a new State.
 * @param def	Initial value (optional).
 */
State::State(const Value& def): first(0, def) {
	OTAWA_CLP_CHECK(locked = false;)
}


/** Copy constructor */
State::State(const State& state): first(0, Value::all){
	OTAWA_CLP_CHECK(locked = false;)	
	copy(state);
}


///
State::~State(void) {
	OTAWA_CLP_CHECK(locked = false;)	
	clear();
}


/**
 * Change the state to be a copy of the given one
 * @param state the state to be copied
*/
void State::copy(const State& state) {
	OTAWA_CLP_CHECK(ASSERT(!locked));
	clear();

	// memory
	first = state.first;
	for(Node *prev = &first, *cur = state.first.next; cur; cur = cur->next) {
		prev->next = new Node(cur);
		prev = prev->next;
	}

	// registers
	registers.copy(state.registers);
	tmpreg.copy(state.tmpreg);
}

/**
 * Remove all nodes from the state
*/
void State::clear(void) {
	OTAWA_CLP_CHECK(ASSERT(!locked));

	// registers
	registers.clear();
	tmpreg.clear();

	// memory
	for(Node *cur = first.next, *next; cur; cur = next) {
		next = cur->next;
		delete cur;
	}
	first.next = 0;
}


/**
 * Set to T the memories on the given area.
 * @param base	Base address of the area.
 * @param size	Size of the area.
 */
void State::clear(t::uint32 base, t::uint32 size) {
	OTAWA_CLP_CHECK(ASSERT(!locked));
	if(first.val == Value::none)
		return;
	for(Node *prev = &first, *cur = first.getNext(), *next; cur; cur = next) {
		next = cur->getNext();
		if(base <= cur->getAddress() && cur->getAddress() < base + size) {
			prev->next = 0;
			delete cur;
		}
		else {
			prev->next = cur;
			prev = cur;
		}
	}
}

/**
 * Define a value into a register or the memory
 * @param addr a value of kind REG for a register, VAL for the memory.
 *		  The value must be a constant (only the lower() attribute will be
 *		  used) or all.
 * @param val the value to store at the given address
*/
void State::set(const Value& addr, const Value& val) {
	OTAWA_CLP_CHECK(ASSERT(!locked));
	Node *prev, *cur, *next;
	if(first.val == Value::none)
		return;

	// insert a none in the state (put the state to none)
	if (val == Value::none){
		clear();
		first.val = Value::none;
		return;
	}

	// assign to register
	if(addr.kind() == REG) {
		if (addr.lower() < 0){
			// temp ones
			if (-addr.lower() < tmpreg.length())
				tmpreg.set(-addr.lower(), val);
			else {
				for(int i = tmpreg.length(); i < -addr.lower(); i++)
					tmpreg.add(Value::all);
				tmpreg.add(val);
			}
		} else {
			// real ones
			if (addr.lower() < registers.length())
				registers.set(addr.lower(), val);
			else {
				for(int i = registers.length(); i < addr.lower(); i++)
					registers.add(Value::all);
				registers.add(val);
			}
		}
		return;
	}

	// consume all memory references
	if(addr == Value::all) {
		prev = &first;
		cur = first.next;
		while(cur) {
			next = cur->next;
			delete cur;
			cur = next;
		}
		prev->next = 0;
		return;
	}

	else if(!addr.isConst()) {
		prev = &first;
		cur = first.next;
		for(prev = &first, cur = first.next; cur && cur->addr < uintn_t(addr.lower()); prev = cur, cur = cur->next);


		for( ; cur && cur->addr <= uintn_t(addr.upper()); ) {
			prev->next = cur->next;
			Node* toDelte = cur;
			cur = cur->next;
			delete toDelte;
		}
	}

	// find a value
	else {
		for(prev = &first, cur = first.next; cur && cur->addr < uintn_t(addr.lower()); prev = cur, cur = cur->next);
		if(cur && cur->addr == uintn_t(addr.lower())) { // find the exact match
			if(val.kind() != ALL)
				cur->val = val;
			else {
				prev->next = cur->next;
				delete cur;
			}
		}
		else if(val.kind() != ALL) { // if not, insert the memory node
			next = new Node(addr.lower(), val);
			prev->next = next;
			prev->next->next = cur;
		}
	}
}


/**
 * @return if a state is equals to the current one
*/
bool State::equals(const State& state) const {

	// Registers
	if (registers.length() != state.registers.length())
		return false;

	for (int i=0; i < registers.length(); i++)
		if (registers[i] != state.registers[i])
			return false;

	// Memory
	if(first.val.kind() != state.first.val.kind())
		return false;

	Node *cur = first.next, *cur2 = state.first.next;
	while(cur && cur2) {
		if(cur->addr != cur2->addr)
			return false;
		if(cur->val != cur2->val)
			return false;

		cur = cur->next;
		cur2 = cur2->next;
	}
	return cur == cur2;
}


/**
 * Test if the current state is a subset of the passed one.
 * @param s		State to test with.
 * @return		True if current state is a subset of the given one, false else.
 */
bool State::subsetOf(const State& s) const {
	if(first.val.kind() == NONE)
		return true;
	else if(s.first.val.kind() == NONE) {
		OTAWA_CLP_CHECK(cerr << "DEBUG: s2 empty!" << io::endl;)
		return false;
	}

	// registers
	if (registers.length() > s.registers.length()) {
		OTAWA_CLP_CHECK(cerr << "DEBUG: too many registers in s2" << io::endl);
		return false;
	}
	for (int i=0; i < registers.length(); i++)
		if(!registers[i].subsetOf(s.registers[i])) {
			OTAWA_CLP_CHECK(cerr << "DEBUG: fail on R" << i << io::endl;)
			return false;
		}

	// memory
	if(first.val.kind() != s.first.val.kind()) {
		OTAWA_CLP_CHECK(cerr << "DEBUG: bad memory base" << io::endl;)
		return false;
	}

	Node *cur = first.next, *cur2 = s.first.next;
	while(cur && cur2) {
		if(cur->addr != cur2->addr) {
			if(cur->addr < cur2->addr)
				cur = cur->next;
			else {
				OTAWA_CLP_CHECK(cerr << "DEBUG: at " << io::hex(cur->addr) << io::endl);
				return false;
			}
		}
		else if(!cur->val.subsetOf(cur2->val)) {
			OTAWA_CLP_CHECK(cerr << "DEBUG: at " << io::hex(cur->addr) << io::endl);
			return false;
		}
		else {
			cur = cur->next;
			cur2 = cur2->next;
		}
	}
	OTAWA_CLP_CHECK(
		if(cur2 != nullptr)
			cerr << "DEBUG: s2 contains mem item not in s1" << io::endl;
	);
	return cur2 == nullptr;
}


/**
 * Merge a state with the current one.
*/
void State::join(const State& state) {
	OTAWA_CLP_CHECK(ASSERT(!locked));

	// test none states
	if(state.first.val == Value::none)
		return;
	if(first.val == Value::none) {
		copy(state);
		return;
	}

	// registers
	for(int i=0; i<registers.length() && i<state.registers.length() ; i++)
		registers[i].join(state.registers[i]);
	if (registers.length() < state.registers.length())
		for(int i=registers.length(); i < state.registers.length(); i++)
			registers.add(state.registers[i]);
	// temp registers
#	ifdef JOIN_TEMP_REGISTERS
	for(int i=0; i<tmpreg.length() && i<state.tmpreg.length() ; i++)
		tmpreg[i].join(state.tmpreg[i]);
	if (tmpreg.length() < state.tmpreg.length())
		for(int i=tmpreg.length(); i < state.tmpreg.length(); i++)
			tmpreg.add(state.tmpreg[i]);
#	endif

	// memory
	Node *prev = &first, *cur = first.next, *cur2 = state.first.next, *next;
	while(cur && cur2) {

		// addr1 < addr2 -> remove cur1
		if(cur->addr < cur2->addr) {
			prev->next = cur->next;
			delete cur;
			cur = prev->next;
		}

		// equality ? remove if join result in all
		else if(cur->addr == cur2->addr) {
			cur->val.join(cur2->val);
			if(cur->val.kind() == ALL) {
				prev->next = cur->next;
				delete cur;
				cur = prev->next;
			}
			else {
				prev = cur;
				cur = cur->next;
				cur2 = cur2->next;
			}
		}

		// addr1 > addr2 => remove cur2
		else
			cur2 = cur2->next;
	}

	// remove tail
	prev->next = 0;
	while(cur) {
		next = cur->next;
		delete cur;
		cur = next;
	}
}

/**
 * Perform a widening.
 * @param state the state of the next iteration
 * @param loopBound is the number of iteration of the loop. A different widening
 *        operation will be used if the loopBound is known (>=0) or not.
*/
void State::widening(const State& state, int loopBound) {
	OTAWA_CLP_CHECK(ASSERT(!locked));

	// test none states
	if(state.first.val == Value::none)
		return;
	if(first.val == Value::none) {
		copy(state);
		return;
	}

	// registers
	for(int i=0; i<registers.length() && i<state.registers.length() ; i++)
		if (loopBound >= 0)
			registers[i].ffwidening(state.registers[i], loopBound);
		else
			registers[i].widening(state.registers[i]);

	if (registers.length() < state.registers.length())
		for(int i=registers.length(); i < state.registers.length(); i++) {
			registers.add(state.registers[i]);
			//registers.add(clp::Value::top);
		}

	// memory
	Node *prev = &first, *cur = first.next, *cur2 = state.first.next, *next;
	while(cur && cur2) {
		// addr1 < addr2 -> remove cur1
		if(cur->addr < cur2->addr) {
			prev->next = cur->next;
			delete cur;
			cur = prev->next;
		}
		// equality ? remove if join result in all
		else if(cur->addr == cur2->addr) {
			if (loopBound >= 0)
				cur->val.ffwidening(cur2->val, loopBound);
			else
				cur->val.widening(cur2->val);
			if(cur->val.kind() == ALL) {
				prev->next = cur->next;
				delete cur;
				cur = prev->next;
			}
			else {
				prev = cur;
				cur = cur->next;
				cur2 = cur2->next;
			}
		}
		else
			cur2 = cur2->next;
	}

	// remove tail
	prev->next = 0;
	while(cur) {
		next = cur->next;
		delete cur;
		cur = next;
	}

}


///
void State::augment(const State& state) {
	OTAWA_CLP_CHECK(ASSERT(!locked));

	// nothing to augment
	if(state.first.val == Value::none)
		return;
	// nothing for *this, hence take all from the augment
	if(first.val == Value::none) {
		copy(state);
		return;
	}

	// registers
	for(int i=0; i<registers.length() && i<state.registers.length() ; i++) {
		if((registers[i] != clp::Value::all) && (state.registers[i] != clp::Value::all))
			registers[i].join(state.registers[i]);
		else if (registers[i] == clp::Value::all)
			registers[i] = state.registers[i];
		else if (state.registers[i] == clp::Value::all)
			{}
		else
			registers[i].join(state.registers[i]);
	}

	if (registers.length() < state.registers.length())
		for(int i=registers.length(); i < state.registers.length(); i++)
			registers.add(state.registers[i]);
	// temp registers
#	ifdef JOIN_TEMP_REGISTERS
	for(int i=0; i<tmpreg.length() && i<state.tmpreg.length() ; i++)
		tmpreg[i].join(state.tmpreg[i]);
	if (tmpreg.length() < state.tmpreg.length())
		for(int i=tmpreg.length(); i < state.tmpreg.length(); i++)
			tmpreg.add(state.tmpreg[i]);
#	endif

	// memory
	Node *cur = first.next, *cur2 = state.first.next; // *prev = &first,
	while(cur && cur2) {

		// addr1 < addr2 -> keep
		if(cur->addr < cur2->addr) {
			cur = cur->next;
		}

		// equality ? remove if join result in all
		else if(cur->addr == cur2->addr) {
			cur->val.join(cur2->val);
			cur = cur->next;
			cur2 = cur2->next;
		}

		// addr1 > addr2 => remove cur2
		else
			cur2 = cur2->next;
	}
}


/**
 * Print the state, the printing does not include the newline at the end
*/
void State::print(io::Output& out, const hard::Platform *pf) const {
	if(first.val == Value::none)
		out << "None (bottom)";
	else {
		#ifdef STATE_MULTILINE
			#define CLP_START "\t"
			#define CLP_END "\n"
			//out << "{\n";
		#else
			#define CLP_START ""
			#define CLP_END ", "
			out << "{";
		#endif
		// tmp registers
		/*for(int i = 0; i < tmpreg.length(); i++){
			Value val = tmpreg[i];
			if (val.kind() == VAL)
				out << CLP_START << "t" << i << " = " << val << CLP_END;
		}*/
		bool fst = true;
		// registers
		for(int i = 0; i < registers.length(); i++){
			Value val = registers[i];
			if (val.kind() == VAL) {
				if(!fst)
					out << ", ";
				else
					fst = false;
				if(!pf)
					out << "r" << i;
				else
					out << pf->findReg(i)->name();
				out << " = " << val;
			}
		}

		// memory
		for(Node *cur = first.next; cur; cur = cur->next) {
			if(!fst)
				out << ", ";
			else
				fst = false;
			out << CLP_START << Address(cur->addr);
			out << " = " << cur->val;
		}

#		if 0
		// temp register, not in state
		for(int i = 0; i < tmpreg.length(); i++){
			Value val = tmpreg[i];
			if (val.kind() == VAL) {
				if(!fst)
					out << ", ";
				else
					fst = false;
				out << "t" << i;
				out << " = " << val;
			}
		}
#		endif
		#ifndef STATE_MULTILINE
		out << "}";
		#endif
	}
}

void State::print(io::StructuredOutput& out, const hard::Platform *pf) const {
	out.beginMap();
	out.key("bottom");
	out.write(first.val == Value::none);
	if(first.val != Value::none) {
		
		// print registers
		out.key("regs");
		out.beginMap();
		for(int i = 0; i < registers.length(); i++){
			Value val = registers[i];
			if (val.kind() == VAL) {
				if(pf != nullptr)
					out.key(_ << 'R' << i);
				else
					out.key(pf->findReg(i)->name());
				out.write(_ << val);
			}
		}
		out.endMap();
		
		// print memory
		out.key("mem");
		out.beginMap();
		for(Node *cur = first.next; cur; cur = cur->next) {
			out.key(_ << Address(cur->addr));
			out.write(_ << cur->val);
		}
		out.endMap();
	}
	out.endMap();
}


/**
 * Return a stored value
 * @param addr is the addresse to get the value of. The kind of the value
 *        can be REG for a register, or VAL for memory. The address is
 *        considered as constant, the lower() attribute is the value.
 * @return the stored value
*/
const Value& State::get(const Value& addr) const {
	Node * cur;
	ASSERTP(addr.isConst(), "addr = " << addr << " is not a constant of the kind " << addr.kind()); // we assume that addr is a constant...
	if(addr.kind() == REG){
		// Tmp Registers
		if (addr.lower() < 0)
			if ((-addr.lower()) < tmpreg.length())
				return tmpreg[-addr.lower()];
			else
				return Value::all;
		// Real registers
		else if (addr.lower() < registers.length())
			return registers[addr.lower()];
		else
			return Value::all;
	} else {
		// Memory
		for(cur = first.next; cur && cur->addr < uintn_t(addr.lower()); cur = cur->next)
				;
		if(cur && cur->addr == uintn_t(addr.lower()))
			return cur->val;
		return first.val;
	}
}

State State::EMPTY(Value::none), State::FULL(Value::all);
	
	
}} // otawa::clp
