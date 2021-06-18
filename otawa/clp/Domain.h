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
#ifndef OTAWA_CLP_DOMAIN_H
#define OTAWA_CLP_DOMAIN_H

#include <otawa/ai/Domain.h>
#include <otawa/clp/State.h>
#include <otawa/dfa/State.h>
#include <otawa/flowfact/features.h>
#include <elm/avl/Map.h>

namespace otawa { namespace clp {

class ContextStack;
	
class Domain: public ai::Domain {
public:

	typedef enum {
		BOTH,
		NOT_TAKEN, 
		TAKEN
	} branch_t;

	Domain(Process *proc);
	~Domain();

	// initialization
	inline void setInitialState(dfa::State* ds) { istate = ds; }
	void initialize(const hard::Register *reg, const Address& address);
	void initialize(const hard::Register *reg, const dfa::Value val);
	void initialize(Address addr, const dfa::Value val);

	// Domain implementation
	ai::State *bot() override;
	ai::State *top() override;
	ai::State *entry() override;
	bool equals(ai::State *s1, ai::State *s2) override;
	ai::State *join(ai::State *s1, ai::State *s2) override;
	ai::State *update(Edge *e, ai::State *s) override;
	ai::State *update(Block *v, ai::State *s) override;
	bool implementsPrinting() override;
	void print(ai::State *s, io::Output& out) override;
	bool implementsCodePrinting() override;
	void printCode(Block *b, io::Output& out) override;
	void printCode(Edge *e, io::Output& out) override;
	bool implementsTracing() override;
	void printTrace(ai::State *s, io::StructuredOutput& out) override;
	
	clp::STAT_UINT
		_nb_inst, _nb_sem_inst,
		_nb_set, _nb_top_set,
		_nb_store, _nb_top_store,
		_nb_top_store_addr,
		_nb_load, _nb_top_load,
		_nb_load_top_addr, _nb_filters,
		_nb_top_filters;

	State *update(const BaseBundle<BasicBlock::InstIter>& b, State *s, branch_t select = BOTH);
	State *update(Inst *inst, int sem, State *s, branch_t select = BOTH);
	
private:
	
	void set(clp::State& state, int i, const clp::Value& v);
	void update(branch_t select);
	void doLoad(State& s, const sem::inst& i);
	void doStore(State& s, const sem::inst& i);
	//void collectInfo(State& s, Vector<FlowFactStateInfo>& ffs);
	
	sem::Block b;
	bool has_if;
	bool has_branch;
	Block *bb; // use for tracking, nothing to do with the analysis itself
	Inst *currentInst; // use for tracking, nothing to do with the analysis itself

	/* attribute for specific analysis / packing */
	bool bBuildFilters; // currently only set to true when building filters
	Process *_process;
	dfa::State *istate;
	clp::Value currentAccessAddress;
	State *cs, *fs, *init;
	ContextStack *stack;
	avl::Map<Block *, State *> loop_states;
};

} } 	// otawa::clp

#endif	// OTAWA_CLP_DOMAIN_H
