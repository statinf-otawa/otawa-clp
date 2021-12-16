/*
 *	predicates module interface
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
#ifndef OTAWA_PRED_PREDICATES_H
#define OTAWA_PRED_PREDICATES_H

#include <functional>
#include <elm/io.h>
#include <elm/data/Vector.h>
#include <otawa/sem/inst.h>
#include <elm/data/HashMap.h>
#include <otawa/cfg.h>
#include <otawa/proc.h>
#include "expression.h"

namespace otawa { namespace pred {

using namespace elm;
using namespace otawa;

class ExpressionManager;

class Predicate {
public:
	virtual ~Predicate();
	static Predicate *reg(int reg, sem::cond_t op, const Expression *e);
	static Predicate *mem(const Expression *a, sem::type_t t, sem::cond_t op, const Expression *e);
	inline sem::cond_t condition() const { return c; }
	inline const Expression *expression() const { return e; }
	virtual void print(io::Output &out) const;
	virtual Predicate *copy() const = 0;
	virtual bool defines(int reg) const = 0;
	virtual bool definesMem() const = 0;
	virtual bool definesAnyReg() const = 0;
	virtual bool contains(int reg) const = 0;
	virtual bool containsMem() const = 0;
	virtual void gen(sem::Block &b) = 0;
	virtual int definedReg() const = 0;
	virtual void substitute(int r, const Expression *expr);
protected:
	Predicate(sem::cond_t cond, const Expression *expr);

private:
	sem::cond_t c;
	const Expression *e;
};

inline io::Output &operator<<(io::Output &out, const Expression *e) {
	e->print(out);
	return out;
}

class Conjunct {
public:
	~Conjunct();
	inline void add(Predicate *pred) { ps.add(pred); }
	inline const Vector<Predicate *> &predicates() const { return ps; }
	void print(io::Output &out) const;
	Conjunct *copy() const;
	const Expression *definitionOf(int reg);
	void map(std::function<Predicate *(Predicate *)> f);
	void gen(sem::Block &b);
private:
	Vector<Predicate *> ps;
};

class Disjunct {
public:
	~Disjunct();
	void add(Conjunct *conj);
	void add(Predicate *pred);
	inline const Vector<Conjunct *> &conjunctions() const { return cs; }
	void print(io::Output &out) const;
	Disjunct *copy() const;
private:
	Vector<Conjunct *> cs;
};

/**
 * Interface to get filters to apply to a value analysis state to improve its
 * precision based on BB predicate building.
 * @ingroup pred
 */
class FilterMaker {
public:
	virtual const otawa::sem::Block& takenFilter(otawa::Block *v) = 0;
	virtual const sem::Block& notTakenFilter(otawa::Block *v) = 0;
	virtual int maxTemp() = 0;
};

extern p::interfaced_feature<FilterMaker> FILTER_FEATURE;


} }	// elm::sem

#endif	// OTAWA_PRED_PREDICATES_H
