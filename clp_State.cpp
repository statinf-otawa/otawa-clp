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

#include "otawa/clp/State.h"

#include <otawa/hard/Platform.h>

namespace otawa { namespace clp {
	
/**
 * @class State
 * state for the @ref clp analysis.
 * @ingroup clp
 */

/**
 * Constructors of a new State.
 * @param tmpcnt	Number of temporaries.
 * @param def		Initial value (optional).
 */
State::State(int tmpcnt, const Value& def)
: base(tmpcnt), first(0, def), linked(tmpcnt) {
}


/** Copy constructor */
State::State(const State& state): base(state.base), first(0, Value::none) {
	copy(state);
}


///
State::~State(void) {
	clear();
}


/**
 * Change the state to be a copy of the given one
 * @param state the state to be copied
*/
void State::copy(const State& state) {
	clear();

	// memory
	base = state.base;
	first = state.first;
	for(Node *prev = &first, *cur = state.first.next; cur; cur = cur->next) {
		prev->next = new Node(cur);
		prev = prev->next;
	}

	// registers
	regs = state.regs;
	srs = state.srs;
	linked.resize(state.linked.size());
	linked = state.linked;
}


/**
 * Remove all nodes from the state
*/
void State::clear(void) {

	// registers
	regs.setLength(base);
	srs.clear();

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
 * Repair the mask of registers linked by status registers.
 */
void State::repairSR(int r) {
	for(List<int>::PrecIter i(srs); i(); i++)
		if(regs[*i].r1() == r || regs[*i].r2() == r) {
			regs[*i] = Value::all;
			srs.remove(i);
			if(!i())
				break;
		}
}


/**
 * Repair the list of status registers (and of linked mask).
 */
void State::rebuildSR() {
	srs.clear();
	linked.clear();
	for(int i = 0; i < regs.length(); i++)
		if(regs[i].isComp()) {
			srs.add(i);
			linked.set(regs[i].r1() + base);
			linked.set(regs[i].r2() + base);
		}
}


/**
 * Assign the value x to the register which number is i.
 * @param r		Register index (negative for temporary).
 * @param x		Assigned value.
 */
void State::setReg(int r, const Value& x) {
	if(first.val == Value::none) {
		first.val = Value::bot;
		clear();
		return;
	}
	int i = r + base;
	
	// enlarge if required
	if(i >= regs.length()) {
		linked.resize(i + 1);
		while(regs.length() <= i) {
			linked.clear(regs.length());
			regs.add(Value::all);
		}
	}
	
	// old value is in a comparison
	if(linked[i]) {
		linked.clear(i);
		repairSR(r);
	}
		
	// perform the assignment
	regs[i] = x;

	// a comparison: update linked and srs
	if(x.isComp()) {
		srs.add(i);
		linked.set(x.r1() + base);
		linked.set(x.r2() + base);
	}
}


/**
 * Assign the value x to the address a in memory.
 * @param a		Address to assign to.
 * @param x		Assigned value.
 */
void State::store(const Value& a, const Value& x) {
	Node *prev, *cur, *next;
	if(first.val == Value::none) {
		first.val = Value::bot;
		clear();
		return;
	}

	// insert a none in the state (put the state to none)
	if(x.isNone()){
		clear();
		first.val = Value::none;
		return;
	}

	// assert to T
	if(a.isAll()) {
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

	// assign to an address range
	else if(!a.isConst()) {
		cur = first.next;
		for(prev = &first, cur = first.next;
			cur != nullptr && cur->addr < uintn_t(a.lower());
			prev = cur, cur = cur->next);
		while(cur != nullptr && cur->addr <= uintn_t(a.upper())) {
			prev->next = cur->next;
			Node* toDelte = cur;
			cur = cur->next;
			delete toDelte;
		}
	}

	// assign to a single address
	else {
		for(prev = &first, cur = first.next;
			cur != nullptr && cur->addr < uintn_t(a.lower());
			prev = cur, cur = cur->next);
		if(cur != nullptr && cur->addr == uintn_t(a.lower())) {
			if(x.kind() != ALL)
				cur->val = x;
			else {
				prev->next = cur->next;
				delete cur;
			}
		}
		else if(x.kind() != ALL) {
			next = new Node(a.lower(), x);
			prev->next = next;
			prev->next->next = cur;
		}
	}
}


/**
 * @return if a state is equals to the current one
*/
bool State::equals(const State& state) const {

	// check registers
	int m = min(regs.length(), state.regs.length());
	for(int i = 0; i < m; i++)
		if(regs[i] != state.regs[i])
			return false;
	if(regs.length() > m)
		for(int i = m; i < regs.length(); i++) {
			if(!regs[i].isAll())
				return false;
		}
	else if(state.regs.length() > m)
		for(int i = m; i < state.regs.length(); i++)
			if(!state.regs[i].isAll())
				return false;

	// check memory
	if(first.val.kind() != state.first.val.kind())
		return false;
	Node *cur = first.next, *cur2 = state.first.next;
	while(cur != nullptr && cur2 != nullptr) {
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
	else if(s.first.val.kind() == NONE)
		return false;

	// registers
	int m = min(regs.length(), s.regs.length());
	for(int i = base; i < m; i++)
		if(!regs[i].subsetOf(s.regs[i]))
			return false;
	if(m < s.regs.length())
		for(int i = m; i  < s.regs.length(); i++)
			if(!s.regs[i].isAll())
				return false;
		
	// memory
	Node *cur = first.next, *cur2 = s.first.next;
	while(cur && cur2) {
		if(cur->addr != cur2->addr) {
			if(cur->addr < cur2->addr)
				cur = cur->next;
			else
				return false;
		}
		else if(!cur->val.subsetOf(cur2->val))
			return false;
		else {
			cur = cur->next;
			cur2 = cur2->next;
		}
	}
	return cur2 == nullptr;
}


/**
 * Merge a state with the current one.
*/
void State::join(const State& state) {

	// test none states
	if(state.first.val == Value::none)
		return;
	if(first.val == Value::none) {
		copy(state);
		return;
	}

	// registers
	int m = min(regs.length(), state.regs.length());
	for(int i = base; i < m; i++)
		regs[i].join(state.regs[i]);
	for(int i = m; i < state.regs.length(); i++)
		regs.add(state.regs[i]);
	rebuildSR();
	
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

	// test none states
	if(state.first.val == Value::none)
		return;
	if(first.val == Value::none) {
		copy(state);
		return;
	}

	// registers
	int m = min(regs.length(), state.regs.length());
	if (loopBound >= 0)
		for(int i = base; i < m; i++)
			regs[i].ffwidening(state.regs[i], loopBound);
	else
		for(int i = base; i < m; i++)
			regs[i].widening(state.regs[i]);
	rebuildSR();
		
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

	// nothing to augment
	if(state.first.val == Value::none)
		return;
	// nothing for *this, hence take all from the augment
	if(first.val == Value::none) {
		copy(state);
		return;
	}

	// registers
	int m = min(regs.length(), state.regs.length());
	for(int i = base; i < m; i++) {
		if(regs[i] != Value::all && state.regs[i] != Value::all)
			regs[i].join(state.regs[i]);
		else if(regs[i] == Value::all)
			regs[i] = state.regs[i];
		else if (state.regs[i] != Value::all)
			regs[i].join(state.regs[i]);
	}

	if (m < state.regs.length())
		for(int i = m; i < state.regs.length(); i++)
			regs.add(state.regs[i]);

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
 * Print the state.
 * @param out	Output stream.
 * @param pf	Current platform.
*/
void State::print(io::Output& out, const hard::Platform *pf) const {
	if(first.val == Value::none)
		out << "{ none }";
	else {
		out << "{ ";
		bool fst = true;
		
		// registers
		for(int i = base; i < regs.length(); i++){
			auto val = regs[i];
			if(val.kind() != ALL) {
				if(!fst)
					out << ", ";
				else
					fst = false;
				if(!pf)
					out << "r" << (i - base);
				else
					out << pf->findReg(i - base)->name();
				out << " = " << val;
			}
		}

		// memory
		for(Node *cur = first.next; cur; cur = cur->next) {
			if(!fst)
				out << ", ";
			else
				fst = false;
			out << Address(cur->addr) << " = " << cur->val;
		}

		// SR/linked print
#		if 0
			if(!fst)
				out << ", ";
			out << "SR [";
			for(auto s: srs)
				out << (s < base ? 't' : 'r') << ::abs(s - base) << ' ';
			out << "], L=[";
			for(int i = 0; i < linked.size(); i++)
				if(linked[i])
					out << (i < base ? 't' : 'r') << ::abs(i - base) << ' ';
			out << "]";
			out << " }";
#		endif
	}
}


/**
 * Output the state to structured output (JSON-like output).
 * @param out	Structured output.
 * @param pf	Current platform.
 */
void State::print(io::StructuredOutput& out, const hard::Platform *pf) const {
	out.beginMap();
	out.key("bottom");
	out.write(first.val == Value::none);
	if(first.val != Value::none) {
		
		// print registers
		out.key("regs");
		out.beginMap();
		for(int i = base; i < regs.length(); i++){
			auto val = regs[i];
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
 * Get a register value.
 * @param r		Register number.
 * @return		REegister value.
 */
const Value& State::getReg(int r) const {
	int i = r + base;
	if(i >= regs.length())
		return Value::all;
	else
		return regs[i];
}


/**
 * Get a memory value.
 * @param addr	Memory address (must be a constant).
 * @return		Memory value.
*/
const Value& State::load(const Value& addr) const {
	Node * cur;
	ASSERTP(addr.isConst(), "addr = " << addr << " is not a constant of the kind " << addr.kind());
	for(cur = first.next; cur && cur->addr < uintn_t(addr.lower()); cur = cur->next)
		;
	if(cur && cur->addr == uintn_t(addr.lower()))
		return cur->val;
	else
		return first.val;
}
	
} } // otawa::clp
