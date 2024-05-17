/*
 *	otawa::clp module features
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2013, IRIT UPS.
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
#ifndef OTAWA_CLP_FEATURES_H_
#define OTAWA_CLP_FEATURES_H_

#include <otawa/proc.h>
#include <otawa/cfg/features.h>

#include "Value.h"

namespace otawa {

class Inst;
namespace hard { class Register; };
	
namespace clp {

extern p::id<bool> USE_FLOWFACT_STATE;

class Problem;
class State;

class ObservedState {
	friend class Analysis;
private:
	inline ObservedState(): state(nullptr), istate(nullptr), bb(nullptr), inst(nullptr) {}
	State *state, *istate;
	BasicBlock *bb;
	Inst *inst;
};

class Manager {
public:
	virtual ~Manager();
	virtual ObservedState *at(BasicBlock *bb, Inst *inst = nullptr, int sem = 0, ObservedState *s = nullptr) = 0;
	virtual void release(ObservedState *s) = 0;
	virtual const Value& valueOf(ObservedState *state, int reg) = 0;
	virtual const Value& valueOf(ObservedState *state, hard::Register *reg) = 0;
	virtual const Value& valueOf(ObservedState *state, const Value& addr) = 0;
	virtual Address stack() const = 0;
};

extern p::interfaced_feature<Manager> ANALYSIS_FEATURE;
extern p::id<bool> TRACE;

} }		// otawa::clp

#endif /* OTAWA_CLP_FEATURES_H_ */

