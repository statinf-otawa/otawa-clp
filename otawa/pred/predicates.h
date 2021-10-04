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

namespace otawa { namespace pred {

using namespace elm;

class Expression;

class Expression {
public:
	typedef enum {
		NONE,
		CST,
		REG,
		MEM,
		MON,
		BIN
	} kind_t;
	
	virtual ~Expression();
	static Expression *reg(int r);
	static Expression *mem(Expression *a, sem::type_t t);
	static Expression *cst(t::uint32 k);
	static Expression *op(sem::opcode op, Expression *a);
	static Expression *op(sem::opcode op, Expression *a1, Expression *a2);

	virtual kind_t kind() const = 0;
	virtual bool contains(int r) const = 0;
	virtual bool containsMem() const = 0;
	virtual Expression *substitute(int r, Expression *e) = 0;

	virtual void gen(sem::Block& b, int t) const = 0;
	virtual Expression *copy() const = 0;
	virtual void print(io::Output& out, int pri = 17) const = 0;
	virtual bool equals(const Expression *e) const = 0;
	virtual int compare(Expression *e) const = 0;
	
	inline bool operator==(const Expression *e) const { return equals(e); }
	inline bool operator!=(const Expression *e) const { return !equals(e); }
};

class Predicate {
public:
	virtual ~Predicate();
	static Predicate *reg(int reg, sem::cond_t op, Expression *e);
	static Predicate *mem(Expression *a, sem::type_t t, sem::cond_t op, Expression *e);
	inline sem::cond_t condition() const { return c; }
	inline Expression *expression() const { return e; }
	void print(io::Output& out) const;
	virtual Predicate *copy() const = 0;
	virtual bool defines(int reg) const = 0;
	virtual bool definesMem() const = 0;
	virtual bool contains(int reg) const = 0;
	virtual bool containsMem() const = 0;
	virtual void gen(sem::Block& b) = 0;
	virtual int definedReg() const = 0;
	virtual void substitute(int r, Expression *e);
protected:
	Predicate(sem::cond_t cond, Expression *expr);
private:
	sem::cond_t c;
	Expression *e;
};
inline io::Output& operator<<(io::Output& out, Expression *e) { e->print(out); return out; }

class Conjunct {
public:
	~Conjunct();
	inline void add(Predicate *pred) { ps.add(pred); }
	inline const Vector<Predicate *>& predicates() const { return ps; }
	void print(io::Output& out) const;
	Conjunct *copy() const;
	Expression *definitionOf(int reg);
	void map(std::function<Predicate *(Predicate *)> f);
	void gen(sem::Block& b);
private:
	Vector<Predicate *> ps;
};

class Disjunct {
public:
	~Disjunct();
	void add(Conjunct *conj);
	void add(Predicate *pred);
	inline const Vector<Conjunct *>& conjunctions() const { return cs; }
	void print(io::Output& out) const;
	Disjunct *copy() const;
private:
	Vector<Conjunct *> cs;
};

}}	// otawa::sem

#endif	// OTAWA_PRED_PREDICATES_H
