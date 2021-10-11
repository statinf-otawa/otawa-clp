/*
 *	predicates module implementation
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

#include <otawa/pred/predicates.h>

namespace otawa { namespace pred {


struct {
	int pri;
	cstring op;
} ops[] = {
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "~" },
	{ -1, "~+" },
	{  6, "+" },
	{  6, "-" },
	{  7, "<<" },
	{  7, ">>+" },
	{  7, ">>" },
	{  3, "-" },
	{  3, "~" },
	{ 11, "&" },
	{ 13, "|" },
	{ 12, "^" },
	{  5, "*" },
	{  5, "*+" },
	{  5, "/" },
	{  5, "/+" },
	{  5, "%" },
	{  5, "%+" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" },
	{ -1, "" }
};

ExpressionManager Expression::expr_manager = ExpressionManager();

class Const: public Expression {
public:
	inline explicit Const(t::uint32 c): k(c) {}
	bool contains(int r) const override { return false; }
	bool containsMem() const override { return false; }
	const  Expression* substitute(int r, const Expression* e) const override  { return this; }
	void gen(sem::Block& b, int t) const override { b.add(sem::seti(t, k)); }
	void print(io::Output& out, int pri = 0) const override  { out << k; }
	bool equals(const Expression* e) const override
		{ auto ee = dynamic_cast<const Const *>((const Expression*)e); return ee != nullptr && k == ee->k; }
	kind_t kind() const override { return CST; }
	int compare(const Expression* e) const override {
		int r = CST - e->kind();
		if(r != 0)
			return r;
		else
			return k - static_cast<Const *>((Expression*)e)->k;
	}
private:
	t::uint32 k;
};

class Reg: public Expression {
public:
	inline explicit Reg(int num): n(num) {}
	bool contains(int r) const override { return n == r; }
	bool containsMem() const override { return false; }
    //ZHEN: delete this????
	const Expression* substitute(int r, const Expression* e) const override
		{ if(r != n) return this; else return e; } //TODO should i delete this?
	void gen(sem::Block& b, int t) const override { b.add(sem::set(t, n)); }
	void print(io::Output& out, int pri) const override
		{ if(n >= 0) out << "R" << n; else out << "T" << n; }
	bool equals(const Expression* e) const override
		{ auto ee = dynamic_cast<const Reg *>((const Expression*)e); return ee != nullptr && n == ee->n; }
	kind_t kind() const override { return REG; }
	int compare(const Expression* e) const override {
		int r = REG - e->kind();
		if(r != 0)
			return r;
		else
			return n - static_cast<Reg *>((Expression*)e)->n;
	}
private:
	int n;
};

class Mem: public Expression {
public:
	inline Mem(const Expression* addr, sem::type_t type): a(addr), t(type) {}
	bool contains(int r) const override { return a->contains(r); }
	bool containsMem() const override { return true; }
	
	const Expression* substitute(int r, const Expression* e) const override {
        return Expression::mem(a->substitute(r, e), t);
	}
	
	void gen(sem::Block& b, int t_) const override {
		a->gen(b, t_);
		b.add(sem::load(t_, t_, t));
	}
	
	void print(io::Output& out, int pri) const override
		{ out << '*'; a->print(out, 3); }

	bool equals(const Expression* e) const override {
		auto ee = dynamic_cast<const Mem *>((const Expression*)e);
		return ee != nullptr && a == ee->a && t == ee->t;
	}

	kind_t kind() const override { return MEM; }
	
	int compare(const Expression* e) const override {
		int r = MEM - e->kind();
		if(r == 0) {
			r = t - static_cast<Mem *>((Expression*)e)->t;
			if(r == 0)
				r = a->compare(static_cast<const Mem *>(e)->a);
		}
		return r;
	}

private:
	const Expression* a;
	sem::type_t t;
};


class Monadic: public Expression {
public:
	inline Monadic(sem::opcode op, const Expression* arg): o(op), a(arg) {}
	~Monadic() = default;
	bool contains(int r) const override { return a->contains(r); }
	bool containsMem() const override { return a->containsMem(); }
	
	const Expression* substitute(int r, const Expression* e)const override {
		return Expression::op(o, a->substitute(r, e));
	}
	
	void gen(sem::Block& b, int t) const override {
		a->gen(b, t);
		b.add(sem::inst(o, t, t));
	}
	
	void print(io::Output& out, int p) const override  {
		auto mp = ops[o].pri;
		if(p <= mp) out << '(';
		out << ops[o].op;
		a->print(out, mp);
		if(p <= mp) out << ')';
	}

	bool equals(const Expression* e) const override {
		auto ee = dynamic_cast<const Monadic *>((const Expression*)e);
		return ee != nullptr && o == ee->o && a == ee->a;
	}

	kind_t kind() const override { return MON; }
	
	int compare(const Expression* e) const override {
		int r = MON - e->kind();
		if(r == 0) {
			r = o - static_cast<Monadic *>((Expression*)e)->o;
			if(r == 0)
				r = a->compare(static_cast<Monadic *>((Expression*)e)->a);
		}
		return r;
	}

private:
	sem::opcode o;
	const Expression* a;
};


class Dyadic: public Expression {
public:
	inline Dyadic(sem::opcode op, const Expression* arg1, const Expression* arg2)
		: o(op), a1(arg1), a2(arg2) {}
	~Dyadic() = default;
	bool contains(int r) const override
		{ return a1->contains(r) || a2->contains(r); }
	bool containsMem() const override
		{ return a1->containsMem() || a2->containsMem(); }
	
	const Expression* substitute(int r, const Expression* e)const override {
        return Expression::op(o, a1->substitute(r,e), a2->substitute(r,e));
	}
	
	void gen(sem::Block& b, int t) const {
		a2->gen(b, t);
		a2->gen(b, t + 1);
		b.add(sem::inst(o, t, t, t + 1));
	}
	
	void print(io::Output& out, int p) const override  {
		auto mp = ops[o].pri;
		if(p <= mp) out << '(';
		a1->print(out, mp);
		out << ' ' << ops[o].op << ' ';
		a2->print(out, mp - 1);
		if(p <= mp) out << ')';
	}

	bool equals(const Expression* e) const override {
		auto ee = dynamic_cast<const Dyadic *>((const Expression*)e);
		return ee != nullptr && o == ee->o && a1 == ee->a1 && a2 == ee->a2;
	}

	kind_t kind() const override { return BIN; }
	
	int compare(const Expression* e) const override {
		int r = BIN - e->kind();
		if(r == 0) {
			r = o - static_cast<Dyadic *>((Expression*)e)->o;
			if(r == 0) {
				r = a1->compare(static_cast<Dyadic *>((Expression*)e)->a1);
				if(r == 0)
					r = a2->compare(static_cast<Dyadic *>((Expression*)e)->a2);
			}
		}
		return r;
	}


private:
	sem::opcode o;
	const Expression *a1, *a2;
};


/*
	bool contains(int r) const override { }
	bool containsMem() const override { }
	Expression *substitute(int r, Expression *e) override  {}
	int gen(Block& b, int& t) const override {}
	Expression *copy() const override {}
	void print(io::Output& out, int pri = 0) const override  {}
*/

/**
 * @class Expression
 * Symbolic expession in semantic instruction predicates. Provides the baseline
 * actions appliable to expessions.
 * @ingroup sem
 */

///
Expression::~Expression() {}

ExpressionManager::~ExpressionManager(){for (auto* expr: _unique_tab ) delete expr;};
//TODO check in the hashtable and return
const Expression *ExpressionManager::makeCst(t::uint32 k) {
    Const new_expr {k};
    auto find = _unique_tab.get(&new_expr);
    if (find.some())
        return find.value();
    auto* new_cst = new Const{k};
    _unique_tab.add(new_cst, new_cst);
    return new_cst;
}

const Expression *ExpressionManager::makeReg(int r) {
    Reg new_expr {r};
    auto find = _unique_tab.get(&new_expr);
    if (find.some())
        return find.value();
    auto* new_cst = new Reg{r};
    _unique_tab.add(new_cst, new_cst);
    return new_cst;
}

const Expression *ExpressionManager::makeMem(const Expression* addr, sem::type_t t) {
    Mem new_expr {addr, t};
    auto find = _unique_tab.get(&new_expr);
    if (find.some())
        return find.value();
    auto* new_cst = new Mem{addr, t};
    _unique_tab.add(new_cst, new_cst);
    return new_cst;
}

const Expression *ExpressionManager::makeOp(sem::opcode op, const Expression* e){
    Monadic new_expr {op,e};
    auto find = _unique_tab.get(&new_expr);
    if (find.some())
        return find.value();
    auto* new_cst = new Monadic{op, e};
    _unique_tab.add(new_cst, new_cst);
    return new_cst;
}

const Expression *ExpressionManager::makeOp(sem::opcode op, const Expression* e1, const Expression* e2){
    Dyadic new_expr {op,e1, e2};
    auto find = _unique_tab.get(&new_expr);
    if (find.some())
        return find.value();
    auto* new_cst = new Dyadic{op, e1, e2};
    _unique_tab.add(new_cst, new_cst);
    return new_cst;
}
/**
 * Build an expression for a register read.
 * @param r		Register index.
 * @return		Built expression.
 */
const Expression* Expression::reg(int r) { return expr_manager.makeReg(r); }

/**
 * Build an expression for a memory read.
 * @param a		Memory address.
 * @return		Built expression.
 */
const Expression* Expression::mem(const Expression* a, sem::type_t t) { return expr_manager.makeMem(a, t); }

/**
 * Build an expression for a constant.
 * @param k		Constant value.
 * @return		Built expression.
 */
const Expression* Expression::cst(t::uint32 k) { return expr_manager.makeCst(k); }

/**
 * Build an expression for a monadic expression.
 * @param op	Monoadic operator.
 * @param a		Argument.
 * @return		Built expression.
 */
const Expression* Expression::op(sem::opcode op, const Expression* a)
	{ return expr_manager.makeOp(op, a); }

/**
 * Build an expression for a dyadic expression.
 * @param op	Dyadic operator.
 * @param a1	First argument.
 * @param a2	Second argument.
 * @return		Built expression.
 */
const Expression* Expression::op(sem::opcode op,const Expression* a1, const Expression* a2)
	{ return expr_manager.makeOp(op, a1, a2); }

/**
 * @fn bool Expression::contains(int r) const;
 * Test if the current expression uses the register r.
 * @param r		Register to test for.
 * @return		True if r is contained, false else.
 */

/**
 * @fn bool Expression::containsMem() const;
 * Test if the current expression contains a memory read.
 * @return	True if the expression contains a memory access, false.
 */

/**
 * @fn Expression *Expression::substitute(int r, Expression *e);
 * Substitute the register access r by the expression e.
 * @param r		Register to substitute.
 * @param e		Expression to substitute with.
 * @return		Expression after substitution.
 */

/**
 * @fn int Expression::gen(Block& b, int& t) const;
 * Add to b the code in semantic instructions translating the expression.
 * @param b		Semantic instruction block to complete.
 * @param t		Temporary number used so far (negative).
 * @return		Temporary number containing the result.
 */

/**
 * @fn Expression *Expression::copy() const;
 * Perform a copy of the current expression.
 * @return		Copy of the expression.
 */

/**
 * @fn void Expression::print(io::Output& out, int p) const;
 * Print the current expression.
 * @param out	Output stream to print.
 * @param p		Priority level of the parent.
 */

class RegPredicate: public Predicate {
public:
	RegPredicate(int reg, sem::cond_t op, const Expression* e): Predicate(op, e), r(reg) {}
	Predicate *copy() const override
		{ return new RegPredicate(r, condition(), expression()); }
	bool defines(int reg) const override { return r == reg && condition() == sem::EQ; }
	bool definesMem() const override { return false; }
	bool contains(int reg) const override
		{ return r == reg || expression()->contains(reg); }
	bool containsMem() const override { return expression()->containsMem(); }
	void gen(sem::Block& b) override {
		expression()->gen(b, -1);
		b.add(sem::cmp(-2, r, -1));
		b.add(assume(condition(), -2));
	}
	int definedReg() const override { return r; }
private:
	int r;
};

class MemPredicate: public Predicate {
public:
	MemPredicate(const Expression* addr, sem::type_t type, sem::cond_t op, const Expression* e)
		: Predicate(op, e), a(addr), t(type) {}
	~MemPredicate() = default;
	Predicate *copy() const override {
		return new MemPredicate(a, t, condition(), expression());
	}
	bool defines(int reg) const override { return false; }
	bool definesMem() const override { return true; }
	bool contains(int reg) const override
		{ return a->contains(reg) || expression()->contains(reg); }
	bool containsMem() const override
		{ return a->containsMem() || expression()->containsMem(); }
	void gen(sem::Block& b) override {
		a->gen(b, -1);
		expression()->gen(b, -2);
		b.add(sem::load(-3, -2, t));
		b.add(sem::cmp(-4, -3, -1));
		b.add(sem::assume(condition(), -4));
		b.add(sem::store(-3, -2, t));
	}
	void substitute(int r, const Expression* e) override {
		Predicate::substitute(r, e);
		auto na = a->substitute(r, e);
		//if(na != a)
		//	delete a;
		a = na;
	}
	int definedReg() const override { return -1; }
private:
	const Expression* a;
	sem::type_t t;
};


/**
 * @class Predicate
 * Represents a predication built from semantic instruction. It is made from
 * three items:
 * 	* location 		-- memory element costrained by the predicate (register,
 *					temporary or main memory).
 *  * operator		-- comparison operator
 *  * expression 	-- expression constraining to constrain with.
 * @ingroup pred
 */

///
Predicate::Predicate(sem::cond_t cond, const Expression* expr): c(cond), e(expr){ }

///
Predicate::~Predicate() = default;

/**
 * Build a predicate applied to a register.
 * @param reg	Register apply to.
 * @param op	comparison operator.
 * @param e		Constraining expresssion.
 * @return		Built predicate.
 */
 // TODO ZHEN: why return nullptr?
Predicate *Predicate::reg(int reg, sem::cond_t op, const Expression* e) { return nullptr; }

/**
 * Build a predicate applied to a memory call.
 * @param reg	Register apply to.
 * @param op	comparison operator.
 * @param e		Constraining expresssion.
 * @param t		Type of memory access.
 * @return		Built predicate.
 */
Predicate *Predicate::mem(const Expression* a, sem::type_t t, sem::cond_t op, const Expression* e) { return new MemPredicate(a, t, op, e); }

///
void Predicate::print(io::Output& out) const {
	static cstring ops[] = {
		"_",
		"==",
		"<",
		"<=",
		">=",
		">",
		"T",
		"!=",
		"<+",
		"<=+",
		">=+",
		">+"
	};
	out << ' ' << ops[c] << ' ';
	e->print(out);
}

/**
 * @fn Predicate *Predicate::copy() const;
 * Copy the current predicate.
 * @return	Copy of the current predicate.
 */

/**
 * @fn bool Predicate::defines(int reg) const;
 * Test if the predicate defines the register reg.
 * @param reg	Register to test.
 * @return		True if the register is defined by the predicate.
 */

/**
 * @fn bool Predicate::definesMem() const;
 * Test if the current predicate defines a memory.
 * @return	True if a memory cell is defined, false else.
 */

/**
 * @fn bool Predicate::contains(int reg) const;
 * Test if the current predicates uses the register reg.
 * @param reg	Register to test.
 * @return		True if the register is used, false else.
 */

/**
 * @fn bool Predicate::containsMem() const;
 * Test if the predicate contains a memory access.
 * @return		True if it contains a memory access, false else.
 */

/**
 * int Predicate::gen(Block& b) = 0;
 * Generate code for the current predicate.
 * @param b		Semantic instruction block to complete.
 * @return		Number of used temporaries.
 */

/**
 * @fn int Predicate::definedReg() const;
 * Get the register defined by this predicate, if any.
 * @return	Defined register number or -1.
 */

/**
 * Sustitute in the predicate the register r by the expression expr.
 * @param r		Register to substitute.
 * @param expr		Expression to substitute with.
 */
void Predicate::substitute(int r, const Expression* expr) {
	auto ne = expr->substitute(r, expr);
	//if(ne != expr)
	//	delete expr;
	e = ne;
}


/**
 * @class Conjunct
 * Represents a conjunction of predicates.
 * @ingroup sem
 */

///
Conjunct::~Conjunct() { }

/**
 * @fn void Conjunct::add(Predicate *pred);
 * Add a predicate to the conjunct.
 */

/**
 * @fn const Vector<Predicate *>& Conjunct::predicates() const;
 * Get the preidcates in the conjunct.
 */

/**
 * Print the conjunct.
 */
void Conjunct::print(io::Output& out) const {
	if(ps.isEmpty())
		out << "T";
	else {
		ps[0]->print(out);
		for(int i = 1; i < ps.length(); i++) {
			out << " /\\ ";
			ps[i]->print(out);
		}
	}
}

/**
 * Make a copy of the current conjunct.
 */
Conjunct *Conjunct::copy() const {
	auto r = new Conjunct();
	r->ps = ps;
	return r;
}

/**
 * Remove the predicates defining or using reg.
 * @param reg	Register to cancel.
 */
void Conjunct::map(std::function<Predicate *(Predicate *)> f) {
	int l = 0;
	for(int i = 0; i < ps.length(); i++) {
		auto p = f(ps[i]);
		if(p == nullptr)
			delete ps[i];
		else
			ps[l++] = p;
	}
	ps.setLength(l);
}

/**
 * Look for a definition of reg.
 * @param reg		Looked register or temporary number.
 * @return			Found definition or null.
 */
const Expression* Conjunct::definitionOf(int reg) {
	for(auto p: ps)
		if(p->defines(reg))
			return p->expression();
	return nullptr;
}

/**
 * Generate code Representing the conjunct.
 * @param b	Semantic instruction block to complete.
 * @return	Maximum number of used temporaries.
 */
void Conjunct::gen(sem::Block& b) {
	for(auto p: ps)
		p->gen(b);
}


/**
 * @class Disjunct
 * Represents a disjunction of conjunctions.
 * @ingroup sem
 */

///
Disjunct::~Disjunct() {
	for(auto c: cs)
		delete c;
}

/**
 * Add a conjunction (ownership of conjunction transferred).
 * @param conj	Added conjunction.
 */
void Disjunct::add(Conjunct *conj) {
	cs.add(conj);
}

/**
 * Add a predicate (ownership of conjunction transferred).
 * Automatically build a singleton conjunction.
 * @param pred	Added predicate.
 */
void Disjunct::add(Predicate *pred) {
	auto c = new Conjunct();
	c->add(pred);
	cs.add(c);
}

/**
 * @fn const Vector<Conjunct *>& Disjunct::conjunctions() const;
 * Get the list of conjuncts.
 * @return List of conjuncts.
 */

/**
 * Print the disjunct.
 * @param out	Output stream to output to.
 */
void Disjunct::print(io::Output& out) const {
	if(cs.length() == 0)
		out << "_";
	else if(cs.length() == 1)
		cs[0]->print(out);
	else {
		bool f = true;
		for(auto c: cs) {
			if(f)
				f = false;
			else
				out << " /\\ ";
			bool single = c->predicates().length() <= 1;
			if(!single)
				out << "(";
			c->print(out);
			if(!single)
				out << ")";
		}		
	}
}

/**
 * Make a clone of the current disjunct.
 * @return	Clone of the disjunct.
 */
Disjunct *Disjunct::copy() const {
	auto d = new Disjunct();
	for(auto c: cs)
		d->add(c->copy());
	return d;
}

}} // otawa::pred
