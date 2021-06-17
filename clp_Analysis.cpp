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
		// TODO
		return nullptr;
	}

	///
	void release(ObservedState *s) override {
		// TODO
	}
	
	const Value& valueOf(ObservedState *state, int reg) override {
		// TODO
	}

	const Value& valueOf(ObservedState *state, hard::Register *reg) override {
		// TODO
	}
	
	const Value& valueOf(ObservedState *state, const Value& addr) override {
		// TODO
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
		
#	if 0

	// perform analysius
#	ifdef CHECK_PARTIAL_ORDER
		ClpListener list(ws, prob, true);
		prob.list = &list;
#	else
		ClpListener list(ws, prob);
#	endif
	ClpFP fp(list);
	ClpAI cai(fp, *ws);
	cai.solve(cfg);

	// the states actually stored in the listener!
	for(CFGCollection::Iter cfg(coll); cfg(); cfg++) {
		if(logFor(LOG_BB))
			log << "\tCFG " << *cfg << io::endl;
		for(CFG::BlockIter bb = cfg->blocks(); bb(); bb++) {
			STATE_IN(*bb) = *(list.results[cfg->index()][bb->index()]);
			if(logFor(LOG_BB)) {
				log << "\t\t" << *bb << io::endl;
				log << "\t\t\tS = ";
				list.results[cfg->index()][bb->index()]->print(log, ws->process()->platform());
				log << io::endl;
			}
		}
	}

	// process stats
	_nb_inst = prob.get_nb_inst();
	_nb_sem_inst = prob.get_nb_sem_inst();
	_nb_set = prob.get_nb_set();
	_nb_top_set = prob.get_nb_top_set();
	_nb_store = prob.get_nb_store();
	_nb_top_store = prob.get_nb_top_store();
	_nb_top_store_addr = prob.get_nb_top_store_addr();
	_nb_load = prob.get_nb_load();
	_nb_load_top_addr = prob.get_nb_load_top_addr();
	_nb_filters = prob.get_nb_filters();
	_nb_top_filters = prob.get_nb_top_filters();
	_nb_top_load = prob.get_nb_top_load();

	//clockWorkSpace = clock() - clockWorkSpace;
	//if(verbose)
	//	elm::cerr << "CLP Analyse takes " << clockWorkSpace << " micro-seconds for processing " << prob.get_nb_clp_bb_count() << " blocks" << io::endl;

//	watchWorkSpace.stop();
//	otawa::ot::time t = watchWorkSpace.delay();
//	elm::cerr << "CLP Analysew takes " << t << " micro-seconds" << io::endl;
#	endif		
	}
	
#if 0
	Vector<init_t> inits;
#endif
	
private:
	const hard::Memory *mem;
	const hard::Platform *pf;
	Domain *domain;
	ai::CFGAnalyzer *ana;
	bool trace;
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
