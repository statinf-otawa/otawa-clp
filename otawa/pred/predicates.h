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

namespace otawa {
    namespace pred {
        class Expression;
    }
}
namespace elm{
    template<>
    class HashKey<const otawa::pred::Expression*>{
    public:
        t::hash computeHash(const otawa::pred::Expression* key) const {return 0;};
        bool isEqual(const otawa::pred::Expression* key1, const otawa::pred::Expression* key2)const {return true;};
        static  t::hash hash(const otawa::pred::Expression* e){return 0;};
        static  bool equals(const otawa::pred::Expression* e1, const otawa::pred::Expression* e2 ){return 0;}
    };
}

namespace otawa { namespace pred {

using namespace elm;


//typedef OwnedPtr<Expression> Expression*;
//typedef BorrowedPtr<Expression> Expression*;


class ExpressionManager{
public:
    ExpressionManager()= default;;
    ~ExpressionManager();
    const Expression* makeReg(int r);
    const Expression* makeMem(const Expression* addr, sem::type_t t);
    const Expression* makeCst(t::uint32 k);
    const Expression* makeOp(sem::opcode op, const Expression* e);
    const Expression* makeOp(sem::opcode op, const Expression* e1, const Expression* e2);
private:
    HashMap<const Expression*, const Expression*> _unique_tab;
};

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
	static const Expression* reg(int r);
	static const Expression* mem(const Expression* a, sem::type_t t);
	static const Expression* cst(t::uint32 k);
	static const Expression* op(sem::opcode op, const Expression* a);
	static const Expression* op(sem::opcode op, const Expression* a1, const Expression* a2);

	virtual kind_t kind() const = 0;
	virtual bool contains(int r) const = 0;
	virtual bool containsMem() const = 0;
	virtual const Expression* substitute(int r, const Expression* e) const = 0;

	virtual void gen(sem::Block& b, int t) const = 0;
	virtual void print(io::Output& out, int pri = 17) const = 0;
	virtual bool equals(const Expression* e) const = 0;
	virtual int compare(const Expression* e) const = 0;
	
	inline bool operator==(const Expression* e) const { return equals(e); }
	inline bool operator!=(const Expression* e) const { return !equals(e); }

private:

    static ExpressionManager expr_manager;
};

class Predicate {
public:
	virtual ~Predicate();
	static Predicate *reg(int reg, sem::cond_t op, const Expression* e);
	static Predicate *mem(const Expression* a, sem::type_t t, sem::cond_t op, const Expression* e);
	inline sem::cond_t condition() const { return c; }
	inline const Expression* expression() const { return e; }
	void print(io::Output& out) const;
	virtual Predicate *copy() const = 0;
	virtual bool defines(int reg) const = 0;
	virtual bool definesMem() const = 0;
	virtual bool contains(int reg) const = 0;
	virtual bool containsMem() const = 0;
	virtual void gen(sem::Block& b) = 0;
	virtual int definedReg() const = 0;
	virtual void substitute(int r, const Expression* expr);
protected:
	Predicate(sem::cond_t cond, const Expression* expr);
private:
	sem::cond_t c;
	const Expression* e;
};
inline io::Output& operator<<(io::Output& out, const Expression *e) { e->print(out); return out; }

class Conjunct {
public:
	~Conjunct();
	inline void add(Predicate *pred) { ps.add(pred); }
	inline const Vector<Predicate *>& predicates() const { return ps; }
	void print(io::Output& out) const;
	Conjunct *copy() const;
	const Expression* definitionOf(int reg);
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
