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
#ifndef OTAWA_PRED_EXPRSSION_H
#define OTAWA_PRED_EXPRSSION_H

#include <otawa/sem/inst.h>

namespace otawa { namespace pred {

class ExpressionManager;
	
class Address {
public:
	static const int
		NO_REG = 1000,
		TOP = 1001;
	static inline Address top() { return Address(TOP, 0); }
	Address(): _type(sem::NO_TYPE), _reg(NO_REG), _off(0) {}
	Address(int r, t::uint32 o, sem::type_t t = sem::NO_TYPE)
		: _type(t), _reg(r), _off(o) {}
	inline int reg() const { return _reg; }
	inline t::uint32 offset() const { return _off; }
	inline Address operator+(t::uint32 k) { return Address(_reg, _off + k); }
	inline Address operator+(t::int32 k) { return Address(_reg, _off + k); }
	inline Address base(int r, t::uint32 k) {
		if(_reg == NO_REG) return Address(NO_REG, 0);
		else return Address(r, _off + k);
	}
	inline bool operator==(const Address& a) const
		{ return _reg == a._reg && _off == a._off && _type == a._type; }
	inline bool operator!=(const Address& a) const {
		return
			_reg == a._reg && _off != a._off
			&& !(_off <= a._off && a._off < _off + sem::size(sem::type_t(_type)))
			&& !(a._off <= _off && _off < a._off + sem::size(sem::type_t(a._type)));
	}
private:
	t::int16 _type;
	t::int16 _reg;
	t::uint32 _off;
};

class Expression {
public:

	typedef enum {
		NONE,
		CST = 4,
		REG = 3,
		MEM = 2,
		MON = 1,
		BIN = 0
	} kind_t;

	inline Expression(ExpressionManager& _man): man(_man) {}
	virtual ~Expression();
	virtual kind_t kind() const = 0;
	virtual bool contains(int r) const = 0;
	virtual bool containsMem() const = 0;
	virtual const Expression *substitute(int r, const Expression *e) const = 0;
	virtual void gen(sem::Block &b, int t) const = 0;
	virtual void print(io::Output &out, int pri = 17) const = 0;
	virtual bool equals(const Expression *e) const = 0;
	virtual int compare(const Expression *e) const = 0;
	virtual t::hash hash()const = 0;
	inline bool operator==(const Expression *e) const { return equals(e); }
	inline bool operator!=(const Expression *e) const { return !equals(e); }
	virtual Address asAddress() const = 0;
	
protected:
	ExpressionManager& man;
};

class Const: public Expression {
public:
	explicit Const(ExpressionManager& man, t::uint32 c);
	inline t::uint32 value() const { return k; }
	bool contains(int r) const override;
	bool containsMem() const override;
	const  Expression* substitute(int r, const Expression* e) const override;
	void gen(sem::Block& b, int t) const override;
	void print(io::Output& out, int pri = 0) const override;
	bool equals(const Expression* e) const override;
	kind_t kind() const override;
	int compare(const Expression* e) const override;
    t::hash hash() const override;
    Address asAddress() const override;
private:
	t::uint32 k;
};

class Reg: public Expression {
public:
	explicit Reg(ExpressionManager& man, int num);
	inline int number() const { return n; }
	bool contains(int r) const override;
	bool containsMem() const override;
	const Expression* substitute(int r, const Expression* e) const override;
	void gen(sem::Block& b, int t) const override;
	void print(io::Output& out, int pri) const override;
	bool equals(const Expression* e) const override;
	kind_t kind() const override;
	int compare(const Expression* e) const override;
    t::hash hash() const override;
    Address asAddress() const override;
private:
	int n;
};

class Mem: public Expression {
public:
	Mem(ExpressionManager& man, const Expression* addr, sem::type_t type);
	inline const Expression *address() const { return a; }
	inline sem::type_t type() const { return t; }
	bool contains(int r) const override;
	bool containsMem() const override;
	const Expression* substitute(int r, const Expression* e) const override;
	void gen(sem::Block& b, int t_) const override;
	void print(io::Output& out, int pri) const override;
	bool equals(const Expression* e) const override;
	kind_t kind() const override;
	int compare(const Expression* e) const override;
    t::hash hash() const override;
	Address asAddress() const override;
	Address makeAddress() const;
private:
	const Expression* a;
	sem::type_t t;
};


class Monadic: public Expression {
public:
	Monadic(ExpressionManager& man, sem::opcode op, const Expression* arg);
	~Monadic() = default;
	inline sem::opcode opcode() const { return o; }
	inline const Expression *arg() const { return a; }
	bool contains(int r) const override;
	bool containsMem() const override;
	const Expression* substitute(int r, const Expression* e)const override;
	void gen(sem::Block& b, int t) const override;
	void print(io::Output& out, int p) const override;
	bool equals(const Expression* e) const override;
	kind_t kind() const override;
	int compare(const Expression* e) const override;
    t::hash hash()const override;
   	Address asAddress() const override;
private:
	sem::opcode o;
	const Expression* a;
};


class Dyadic: public Expression {
public:
	Dyadic(ExpressionManager& man, sem::opcode op, const Expression* arg1, const Expression* arg2);
	~Dyadic() = default;
	inline sem::opcode opcode() const { return o; }
	inline const Expression *arg1() const { return a1; }
	inline const Expression *arg2() const { return a2; }
	bool contains(int r) const override;
	bool containsMem() const override;
	const Expression* substitute(int r, const Expression* e)const override;
	void gen(sem::Block& b, int t) const;
	void print(io::Output& out, int p) const override;
	bool equals(const Expression* e) const override;
	kind_t kind() const override;
	int compare(const Expression* e) const override;
    t::hash hash() const override;
	Address asAddress() const override;
private:
	sem::opcode o;
	const Expression *a1, *a2;
};

	
}}	// otawa::pred

namespace elm {

	template<>
class HashKey<const otawa::pred::Expression*>{
public:
	t::hash computeHash(const otawa::pred::Expression* key) const {return hash(key);};
	bool isEqual(const otawa::pred::Expression* key1, const otawa::pred::Expression* key2)const {return equals(key1, key2);};
	static  t::hash hash(const otawa::pred::Expression* e){return e->hash();};
	static  bool equals(const otawa::pred::Expression* e1, const otawa::pred::Expression* e2 ){return e1->kind() == e2->kind() && e1->equals(e2);};
};

	
}


namespace otawa { namespace pred {

class ExpressionManager {
public:
	ExpressionManager()= default;
	~ExpressionManager();
	const Expression* makeReg(int r);
	const Mem* makeMem(const Expression* addr, sem::type_t t);
	const Expression* makeCst(t::uint32 k);
	const Expression* makeOp(sem::opcode op, const Expression* e);
	const Expression* makeOp(sem::opcode op, const Expression* e1, const Expression* e2);
private:
	HashMap<const Expression*, const Expression*> _unique_tab;
};

} }	// otawa::pred

#endif	// OTAWA_PRED_EXPRSSION_H
