/*
 *	otawa::clp::Analysis class
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <elm/json/Saver.h>
#include <otawa/clp/features.h>
#include <otawa/clp/State.h>
#include <otawa/cfg/features.h>
#include <otawa/dfa/State.h>
#include <otawa/hard/Memory.h>
#include <otawa/hard/Register.h>
#include <otawa/ipet/features.h>
#include <otawa/ai/CFGAnalyzer.h>
#include <otawa/ai/FlowAwareRanking.h>

#include "otawa/clp/Domain.h"

namespace otawa { namespace clp {

/**
 * @class Manager;
 * Interface allowing the use of the results of the CLP analysis. The user has
 * to choose to which program point the state needs to be looked and obtains
 * the corresponding state. A program point is made of triple (basic block,
 * instruction, semantic instruction index). Then register values or memory
 * values can be accessed. Finally, the obtained state has to be released.
 * 
 * If several values in the same basic block and instruction has to be observed,
 * the returned state can be re-used to avoid recalculating the intermediate
 * states in the basic block.
 * 
 * **Provided by:** @ref ANALYSIS_FEATURE
 * 
 * @ingroup clp
 */

///
Manager::~Manager() { }

/**
 * @fn ObservedState *Manager::at(BasicBlock *bb, Inst *inst = nullptr, int sem = -1, ObservedState *s = nullptr);
 * Get the CLP state at the given program point (basic block, instruction, semantic instruction index).
 * The obtained state may be either be reused in a subsequent call to @ref at(), or @red relased()'d.
 * @param 	bb	Looked BB.
 * @param 	inst	Looked instruction (optional).
 * @param 	sem		Looked semantic instruction index (optional).
 * @param 	state	State to re-use to speed up calculation.
 * @return	State at the given point.
 */

/**
 * @fn void Manager::release(ObservedState *s);
 * Release a state.
 * @param s		Release state.
 */

/**
 * @fn const Value& Manager::valueOf(ObservedState *state, int reg);
 * Get the value of a register in the given state.
 * @param state	State to look in.
 * @param reg	Register to look to (unique identifier).
 * @return		Value of the register.
 */

/**
 * @fn const Value& Manager::valueOf(ObservedState *state, hard::Register *reg);
 * Get the value of a register in the given state.
 * @param state	State to look in.
 * @param reg	Register to look to.
 * @return		Value of the register.
 */

/**
 * @fn const Value& Manager::valueOf(ObservedState *state, const Value& addr);
 * Get a memory value in the given state.
 * @param state	State to look in.
 * @param addr	Address of the looked memory.
 * @return		Value at the given address.
 */


/**
 * Implements CLP analysis.
 */
class Analysis: public BBProcessor, public Manager {
public:

	static p::declare reg;
	
	Analysis(p::declare& r = reg): BBProcessor(reg), trace(false) { }
	
	
#	if 0
	typedef Pair<const hard::Register *, Address> init_t;
	/** Initial state of the analysis */
	static Identifier<init_t> INITIAL;
#	endif

	void *interfaceFor(const AbstractFeature& f) override {
		if(f == ANALYSIS_FEATURE)
			return static_cast<Manager *>(this);
		else
			return nullptr;
	}

	///
	ObservedState *at(BasicBlock *bb, Inst *inst, int sem, ObservedState *s) override {
		
		// if required, create the state
		if(s == nullptr) {
			s = new ObservedState();
			ostates.add(s);
		}
		
		// get the block state
		if(bb != s->bb) {
			s->istate = new State(*static_cast<State *>(ana->before(bb)));
			s->inst = nullptr;
		}
		if(!bb->isBasic())
			return s;
		
		// get the instruction state
		auto i = bb->toBasic()->bundles().begin();
		if(s->inst != nullptr && s->inst->address() < inst->address())
			while(!(*i).area().contains(s->inst->address()))
				i++;
		s->inst = inst;
		while(!(*i).area().contains(s->inst->address())) {
			s->istate = domain->update(*i, s->istate);
			i++;
		}
		
		// go to the semantic instruction
		if(sem == 0)
			s->state = s->istate;
		else {
			s->state = new State(*s->istate);
			s->state = domain->update(s->inst, sem, s->state);
		}
		
		// get the semantic instruction state
		return s;
	}

	///
	void release(ObservedState *s) override {
		ostates.remove(s);
		delete s;
	}
	
	const Value& valueOf(ObservedState *state, int reg) override {
		return state->state->get(Value::R(reg));
	}

	const Value& valueOf(ObservedState *state, hard::Register *reg) override {
		return state->state->get(Value::R(reg->platformNumber()));
	}
	
	const Value& valueOf(ObservedState *state, const Value& addr) override {
		return state->state->get(addr);
	}

protected:

	///
	void configure(const PropList& props) override {
		BBProcessor::configure(props);
		trace = TRACE(props);
	}
	
	///
	void setup(WorkSpace *ws) override {
		domain = new Domain(ws->process());
		
		// get hardware information
		mem = hard::MEMORY_FEATURE.get(ws);
		pf = ws->platform();
		
		// look for initial state
		// TODO all inits should be set using semantic instructions!
		dfa::State *istate = dfa::INITIAL_STATE(ws);
		if(istate != nullptr) {

			// initialize registers
			for(dfa::State::RegIter r(istate); r(); r++)
				domain->initialize((*r).fst, (*r).snd);

			// initialize memory
			for(dfa::State::MemIter m(istate); m(); m++)
				domain->initialize((*m).address(), (*m).value());

			domain->setInitialState(istate);
		}
		
		// ensure SP initialisation
		const hard::Register *sp = ws->process()->platform()->getSP();
		if(!sp)
			warn("no stack pointer in the architecture.");
		else {
			Value v = static_cast<State *>(domain->entry())->get(Value(REG, sp->platformNumber()));
			if(v.isTop()) {
				bool found = false;
				if(mem) {
					warn("no initial for stack pointer: looking in memory.");
					Address addr;
					const Array< const hard::Bank * > &banks = mem->banks();
					for(int i = 0; i < banks.count(); i++)
						if(banks[i]->isWritable()
						&& banks[i]->type() != hard::Bank::IO
						&& (addr.isNull() || banks[i]->address() > addr))
							addr = banks[i]->topAddress();
					if(addr.isNull()) {
						warn("no writable memory: reverting to loader stack address.");
						addr = ws->process()->defaultStack();
					}
					if(!addr.isNull()) {
						warn(_ << "setting stack at " << addr);
						domain->initialize(sp, addr);
						found = true;
					}
				}
				if(!found)
					warn("no value for the initial stack pointer");
			}
		}
	
	}

	void destroy(WorkSpace *ws) override {
		if(ana != nullptr)
			delete ana;
		delete domain;
	}
	
	void processBB(WorkSpace *ws, CFG *g, Block *b) override {
	}
	
	void dumpBB(Block *v, io::Output& out) override {
		domain->print(ana->after(v), out);
	}
	
	void processWorkSpace(WorkSpace *ws) override {
		ana = new ai::CFGAnalyzer(*this, *domain);
		
		// if required, prepare trace
		UniquePtr<io::OutStream> str;
		UniquePtr<json::Saver> out;
		if(trace) {
			try {
				str = Path("clp-trace.json").write();
				out = new json::Saver(*str);
				ana->setTrace(*out);
			}
			catch(sys::SystemException& e) {
				log << "ERROR: cannot trace in clp-trace.json: " << e.message() << io::endl;
			}
		}
		
		// perform the analysis
		ana->process();
		
		// display store-to-T
		auto& stt = domain->topStores();
		if(!stt.isEmpty()) {
			StringBuffer buf;
			buf << "results could be very imprecise because one or several store to T address has been found:\n";
			for(auto i: stt)
				buf << "\t- " << i->address() << " " << i << io::endl;
			warn(buf.toString());
		}
	}
	
private:
	const hard::Memory *mem;
	const hard::Platform *pf;
	Domain *domain;
	ai::CFGAnalyzer *ana;
	bool trace;
	List<ObservedState *> ostates;
};

///
p::declare Analysis::reg = p::init("otawa::clp::Analysis", Version(2, 0, 0))
	.make<Analysis>()
	.require(COLLECTED_CFG_FEATURE)
	.require(LOOP_INFO_FEATURE)
	.require(dfa::INITIAL_STATE_FEATURE)
	.require(hard::MEMORY_FEATURE)
	.require(ipet::FLOW_FACTS_FEATURE)
	.require(ai::CFG_RANKING_FEATURE)
	.provide(clp::ANALYSIS_FEATURE);


/**
 * This features ensure that the clp analysis has been identified.
 *
 * @par Default Processor
 * @li @ref otawa::clp::Analysis
 *
 * @par Configuration
 * @li @ref otawa::clp::TRACE
 *
 * @ingroup clp
 */
p::interfaced_feature<Manager> ANALYSIS_FEATURE("otawa::clp::ANALYSIS_FEATURE", p::make<Analysis>());

/**
 * Configuration property enabling tracing for the CLP analysis.
 */
p::id<bool> TRACE("otawa::clp::TRACE");

} }	// otawa::clp
