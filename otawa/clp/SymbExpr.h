/*
 *	SymbExpr class interface
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

#ifndef OTAWA_CLP_SYMBOLICEXPR_H_
#define OTAWA_CLP_SYMBOLICEXPR_H_

#include <elm/io.h>
#include <elm/data/Vector.h>
#include <otawa/cfg/BasicBlock.h>
#include "State.h"

namespace otawa {

namespace hard { class Platform; }
	
namespace clp {

class Problem;

/** Symbolic expression*/
class SymbExpr {
public:

	typedef enum {
		NONE,
		CONST,		/** Constant */
		ADDR,		/** Memory reference */
		REG,		/** Register */
		NEG,		/** Opposite of */
		ADD,		/** Addition */
		CMP,		/** Undeterminate compare */
		CMPU,		/** Undeterminate unsigned compare */
					/** Determinate compare: */
		LE,			/**		less or equal */
		LT,			/**		less than */
		GE,			/**		greater or equal */
		GT,			/**		greater than */
		EQ,			/**		equal */
		NE,			/**		not equal */
					/**	Unsigned forms: */
		ULE,
		ULT,
		UGE,
		UGT,
		OR
	} op_t;

	SymbExpr(op_t op=NONE, SymbExpr *a=NULL, SymbExpr *b=NULL, Value val=0, SymbExpr *parent=NULL);
	SymbExpr(const SymbExpr &expr);
	virtual ~SymbExpr();

	virtual SymbExpr* copy(void);
	virtual SymbExpr& operator=(const SymbExpr& expr);
	virtual bool operator==(const SymbExpr& expr) const;
	virtual String asString(const hard::Platform *pf = 0);
	virtual void canonize(void);
	virtual Vector<Value> used_reg(void);
	virtual Vector<Value> used_addr(void);
	virtual SymbExpr* solidifyAddress(clp::State& clpState, bool dig);

	inline op_t op(void) const { return _op; }
	inline SymbExpr* a(void) const { return _a; }
	inline SymbExpr* b(void) const { return _b; }
	inline Value val(void) const { return _val; }
	inline SymbExpr* parent(void) const { return _parent; }
	inline void set_parent(SymbExpr *parent) { _parent = parent ; }

	bool operator!=(const SymbExpr& expr) const;
	void replace(SymbExpr *searched_se, SymbExpr *new_se);
	void print(io::Output& out, const hard::Platform *pf = 0);
	void set_a(SymbExpr *a);
	void set_b(SymbExpr *b);

protected:
	op_t reverse(op_t logo);
	op_t _op;
	SymbExpr *_a;
	SymbExpr *_b;
	Value _val;
	SymbExpr *_parent;
};

class SEConst: public SymbExpr {
public:
	inline SEConst(Value value, SymbExpr *parent=NULL):
			SymbExpr(CONST, NULL, NULL, value, parent) {}
	inline SEConst(const SEConst &expr): SymbExpr(expr) {}
	SEConst* copy(void) override;
	SymbExpr& operator=(const SEConst& expr);
	bool operator==(const SymbExpr& expr) const override;
	String asString(const hard::Platform *pf = 0) override;
	void canonize(void) override;
	SymbExpr* solidifyAddress(clp::State& clpState, bool dig) override;
};

class SEAddr: public SymbExpr {
public:
	inline SEAddr(Value value, SymbExpr* a = NULL, SymbExpr* parent = NULL) : SymbExpr(ADDR, a, NULL, value, parent) { }
	SEAddr(const SEAddr &expr): SymbExpr(expr) {}
	SEAddr* copy(void) override;
	SymbExpr& operator=(const SEAddr& expr);
	bool operator==(const SymbExpr& expr) const override;
	String asString(const hard::Platform *pf = 0) override;
	void canonize(void) override;
	Vector<Value> used_addr(void) override;
	SymbExpr* solidifyAddress(clp::State& clpState, bool dig) override;
};

/** Register */
class SEReg: public SymbExpr{
public:
	inline SEReg(Value value, SymbExpr *parent=NULL):
		SymbExpr(REG, NULL, NULL, value, parent) {}
	inline SEReg(const SEReg &expr): SymbExpr(expr) {}
	SEReg* copy(void);
	SymbExpr& operator=(const SEReg& expr);
	bool operator==(const SymbExpr& expr) const override;
	String asString(const hard::Platform *pf = 0) override;
	void canonize(void) override;
	Vector<Value> used_reg(void) override;
	SymbExpr* solidifyAddress(clp::State& clpState, bool dig) override;
};

/** Negation */
class SENeg: public SymbExpr{
public:
	inline SENeg(SymbExpr *expr=NULL, SymbExpr *parent=NULL):
		SymbExpr(NEG, expr, NULL, 0, parent) {}
	inline SENeg(const SENeg &expr): SymbExpr(expr) {}
	SENeg* copy(void) override;
	SymbExpr& operator=(const SENeg& expr);
	bool operator==(const SymbExpr& expr) const override;
	String asString(const hard::Platform *pf = 0) override;
	void canonize(void) override;
	SymbExpr* solidifyAddress(clp::State& clpState, bool dig) override;
};

/** Addition */
class SEAdd: public SymbExpr{
public:
	inline SEAdd(SymbExpr *a=NULL, SymbExpr *b=NULL, SymbExpr *parent=NULL):
		SymbExpr(ADD, a, b, 0, parent) {}
	inline SEAdd(const SEAdd &expr): SymbExpr(expr) {}
	SEAdd* copy(void) override;
	SymbExpr& operator=(const SEAdd& expr);
	bool operator==(const SymbExpr& expr) const override;
	String asString(const hard::Platform *pf = 0) override;
	void canonize(void) override;
	SymbExpr* solidifyAddress(clp::State& clpState, bool dig) override;
};

class SECmp: public SymbExpr{
public:
	inline SECmp(op_t op, SymbExpr *a=NULL, SymbExpr *b=NULL, SymbExpr *parent=NULL):
		SymbExpr(op, a, b, 0, parent) {}
	inline SECmp(const SECmp &expr): SymbExpr(expr) {}
	SECmp* copy(void) override;
	SymbExpr& operator=(const SECmp& expr);
	bool operator==(const SymbExpr& expr) const override;
	String asString(const hard::Platform *pf = 0) override;
	void canonize(void) override;
	SECmp* logicalNot(void);
	bool isValid(void);
};


extern Identifier<Vector<SECmp *> > REG_FILTERS;
extern Identifier<Vector<SECmp *> > ADDR_FILTERS;

class FilterBuilder {
public:
	FilterBuilder(BasicBlock *_bb, Problem& problem);
private:
	void getFilters(void);
	void iterateBranchPaths(const BasicBlock::Bundle& branchBundle, const Vector<BasicBlock::Bundle>& bundles);
	sem::cond_t reverseCond(sem::cond_t cond);
	SECmp *makeFilters(SECmp *se, const BasicBlock::Bundle& currentBundle, sem::Block& b, bool branch);
	void addFilters(SECmp *se, const Vector<BasicBlock::Bundle>& bundles);
	void prepareSemBlockPaths(Vector<sem::Block>& semBlocks, const sem::Block& b);

	BasicBlock *bb;

	// filters for the taken edge
	Vector<SECmp *> reg_filters;
	Vector<SECmp *> addr_filters;

	// filters for the non-taken edge
	Vector<SECmp *> reg_filters_not;
	Vector<SECmp *> addr_filters_not;

	// filters for the currently processing path
	Vector<SECmp *> curr_reg_filters;
	Vector<SECmp *> curr_addr_filters;
	Vector<Value> curr_known_reg;
	Vector<Value> curr_known_addr;
};


/**
	* Apply a filter on the value
	* @param v the CLP to be filtred
	* @param cmp_op compare operator
	* @param f CLP to filter with
*/
void applyFilter(Value &v, SymbExpr::op_t cmp_op, Value f);

inline io::Output& operator<<(elm::io::Output& out, SymbExpr *sym)
	{ out << "=> "; sym->print(out); return out; }

} } // otawa::clp

#if 0
// output
inline elm::io::Output& operator<<(elm::io::Output& out, otawa::clp::SymbExpr &se) {
	se.print(out);
	return out;
}

// used to build sem::Block for different paths when a Block contains if(s)
class SemInstNode {
	int semPC;
	bool parentCondition;
	SemInstNode* parent;
	SemInstNode* lChild;
	SemInstNode* rChild;
public:
	inline ~SemInstNode(void) { }
	inline SemInstNode(int pc, bool b, SemInstNode* p=NULL, SemInstNode* l=NULL, SemInstNode* r=NULL): semPC(pc), parentCondition(b), parent(p), lChild(l), rChild(r) { }
	inline int getPC(void) { return semPC; }
	inline SemInstNode* getParent(void) { return parent; }
	inline bool getCond(void) { return parentCondition; }
};
#endif


#endif /* OTAWA_DATA_CLP_SYMBOLICEXPR_H_ */
