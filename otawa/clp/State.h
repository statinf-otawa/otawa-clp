/*
 *	clp::State class interface
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

#ifndef OTAWA_CLP_STATE_H_
#define OTAWA_CLP_STATE_H_

#include <elm/data/Vector.h>
#include <elm/data/List.h>
#include <elm/util/BitVector.h>
#include <otawa/ai/Domain.h>
#include "Value.h"

namespace otawa {

namespace hard {
	class Platform;
	class Register;
}

namespace clp {

/**
 * The abstract state of the computer (abstract domain). The abstract state
 * is a list of register states and memory states.
 */
class State: public ai::State {
public:
	
	/**
		* A node in the memory list
	*/
	class Node {
	public:
		friend class State;
		inline Node(void): next(0), addr(0), val(ALL) { }
		inline Node(t::uint32 address, const Value& value): next(0), addr(address), val(value) { }
		inline Node(const Node *node): next(0), addr(node->addr), val(node->val) { }
		inline Node *getNext(void) const { return next; }
		inline t::uint32 getAddress(void) const { return addr; }
		inline const Value& getValue(void) const { return val; }

	private:
		Node *next;
		t::uint32 addr;
		Value val;
	};
	
	/** Constructors of a new State	*/
	State(int base, const Value& def = Value::all);
	State(const State& state);
	~State(void);

	inline bool isNone() const { return first.val.isNone(); }
	inline State& operator=(const State& state){copy(state); return *this; }
	inline bool operator==(const State& state) const { return equals(state); }
	
	void copy(const State& state);
	void clear(void);
	void setReg(int r, const Value& x);
	void store(const Value& addr, const Value& val);
	void clear(t::uint32 base, t::uint32 size);
	bool equals(const State& state) const;
	void join(const State& state);
	void widening(const State& state, int loopBound);
	void print(io::Output& out, const hard::Platform *pf = nullptr) const;
	void print(io::StructuredOutput& out, const hard::Platform *pf = nullptr) const;
	const Value& load(const Value& addr) const;
	const Value& getReg(int r) const;
	void augment(const State& state);
	bool subsetOf(const State& s) const;
	
	class Iter: public PreIterator<Iter, const Value&> {
	public:
		inline Iter(const State& s): state(s), i(s.base), node(state.first.getNext()) { }
		inline const Value& item(void) const
			{ if(isReg()) return state.regs[i]; else return node->getValue(); }
		inline void next(void) { if(isReg()) i++; else node = node->getNext(); }
		inline bool ended(void) const { return !isReg() && !node; }
		inline Value id(void) const { if(isReg()) return Value(REG, i - state.base); else return Value(VAL, node->getAddress()); }
	private:
		inline bool isReg(void) const { return i < state.regs.count(); }
		const State& state;
		int i;
		Node *node;
	};

protected:
	
	void repairSR(int r);
	void rebuildSR();
	
	int base;
	Node first;
	Vector<Value> regs;
	List<int> srs;
	BitVector linked;
};

} }	// otawa::clp

#endif /* OTAWA_DATA_CLP_STATE_H_ */
