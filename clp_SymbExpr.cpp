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

#include <otawa/clp/Domain.h>
#include <otawa/clp/SymbExpr.h>
#include <otawa/hard/Platform.h>

namespace otawa { namespace clp {

/* reverse a logical operator (to reverse operand in SECmp */
SymbExpr::op_t SymbExpr::reverse(op_t logop){
	switch(logop){
	case LE:
		return GE;
	case LT:
		return GT;
	case GE:
		return LE;
	case GT:
		return LT;
	case ULE:
		return UGE;
	case ULT:
		return UGT;
	case UGE:
		return ULE;
	case UGT:
		return ULT;
	default:
		// EQ -> EQ; NE -> NE; others (invalid) are returned unchanged
		return logop;
	}
}


/**
 * @class SymbExpr
 * Symbolic expression.
 */

/**
 * Build a new symbolic expression
 * You shoul instaciate childs instead of this expression
*/
SymbExpr::SymbExpr(op_t op, SymbExpr *a, SymbExpr *b, Value val, SymbExpr *parent):
	_op(op), _a(a), _b(b), _val(val), _parent(parent) {
	ASSERT(_op >= NONE && _op <= OR);
	if(_a != NULL)
		_a->set_parent(this);
	if(_b != NULL)
		_b->set_parent(this);
}
	
/**
	* Copy constructor
	* @param expr the expression to copy
*/
SymbExpr::SymbExpr(const SymbExpr &expr):
	_op(expr._op), _val(expr._val), _parent(expr._parent) {
	ASSERT(_op >= NONE && _op <= OR);
	set_a(expr._a); // to copy expr._a
	set_b(expr._b); // to copy expr._b
}

/**
 * Destructor of a symbolic expression
 * This recursively delete a and b childs, and set to NULL the reference
 * of this in the parent (if any).
*/
SymbExpr::~SymbExpr(){
	delete _a;
	delete _b;
	if (_parent != NULL){
		if (_parent->_a == this)
			_parent->_a = NULL;
		else if (_parent->_b == this)
			_parent->_b = NULL;
	}
}

/**
 * Copy operator
 * @return a pointer to a copy of this
 */
SymbExpr* SymbExpr::copy(void) {
	SymbExpr *newa = NULL;
	SymbExpr *newb = NULL;
	if (_a != NULL)
		newa = _a->copy();
	if (_b != NULL)
		newb = _b->copy();
	return new SymbExpr(_op, newa, newb, _val);
}

/**
 * Affectation operator
 * @param expr the expression to be copied in this
 * @return this
 */
SymbExpr& SymbExpr::operator=(const SymbExpr& expr) {
	_op = expr._op;
	set_a(expr._a);
	set_b(expr._b);
	_val = expr._val;
	return *this;
}

/**
 * Equality
 * @param expr the expression to be compared with this
 * @return if the expression reprensents the same as expr (recursively)
 */
bool SymbExpr::operator==(const SymbExpr& expr) const {
	return (_op == expr._op);
}

/**
 * Not equal operator
 * @param expr the expression to be compared with this
 * @return if the expression doesn't reprensents the same as expr
 */
bool SymbExpr::operator!=(const SymbExpr& expr) const {
	return ! operator==(expr);
}

/**
 * @fn op_t SymbExpr::op(void) const;
 * @return the operator of the expression
 */

/**
 * SymbExpr* SymbExpr::a(void) const;
 * @return a pointer to the first child (a) of the expression (or NULL)
*/

/**
 * SymbExpr* SymbExpr::b(void) const;
 * @return a pointer to the second child (b) of the expression (or NULL)
 */

/**
 * V SymbExpr::val(void) const;
 * @return the value of this expression
 */

/**
 * SymbExpr* SymbExpr::parent(void) const;
 * @return a pointer to the parent of this (or NULL)
 */

/**
 * Recursively replace in this the searched_se expression by new_se
 * @param searched_se the researched expression
 * @param new_se the expression that will replace searched_se
 *
 * new_se will be copied each time it replace a searched_se.
*/
void SymbExpr::replace(SymbExpr *searched_se, SymbExpr *new_se) {
	if (_a != NULL && *_a == *searched_se){
		set_a(new_se);
	} else if (_a != NULL /* && *_a != *searched_se */ )
		_a->replace(searched_se, new_se);
	if (_b != NULL && *_b == *searched_se){
		set_b(new_se);
	} else if (_b != NULL /* && *_b != *searched_se */ )
		_b->replace(searched_se, new_se);
}

/**
 * @return a string reprensentation of the expression
 */
String SymbExpr::asString(const hard::Platform *pf) {
	return "[generic]";
} 

/**
 * Output a string representation of the expression
 * @param out the output channel
 * @param pf  current plarform
 */
void SymbExpr::print(io::Output& out, const hard::Platform *pf) {
	out << asString(pf);
}

/**
 * Try to recursively transform the expression into it its canonized
 * form.
 */
void SymbExpr::canonize(void) {
	if (_a)
		_a->canonize();
	if (_b)
		_b->canonize();
}

/**
 * Return the list of registers used
 * (recursively) in this expression.
 * @return a vector of int.
 */
Vector<Value> SymbExpr::used_reg(void) {
	// recursively get reg for sub-expr
	Vector<Value> a_reg, b_reg;
	if (_a)
		a_reg = _a->used_reg();
	if (_b)
		b_reg = _b->used_reg();
	// merge values of the b vector in the a vector
	for(int i=0; i < b_reg.length(); i++)
		if (! a_reg.contains(b_reg[i]))
			a_reg.add(b_reg[i]);
	return a_reg;
}

/**
 * Return the list of memory addresses used
 * (recursively) in this expression.
 * @return a vector of int.
 */
Vector<Value> SymbExpr::used_addr(void) {
	// recursively get addr for sub-expr
	Vector<Value> a_addr, b_addr;

	if (_a)
		a_addr = _a->used_addr();
	if (_b)
		b_addr = _b->used_addr();
	// merge values of the b vector in the a vector
	for(int i=0; i < b_addr.length(); i++)
		if (! a_addr.contains(b_addr[i]))
			a_addr.add(b_addr[i]);

	return a_addr;
}

/**
 * Set the first child expression (a)
 * @param a a pointer to the new first child
 *
 * A *copy* of a will be set as first child.
 * If there was a first child in this expression, it will be delete, but
 * after the copy of a (so it's safe to set_a() a child expression of
 * this->a()).
 */
void SymbExpr::set_a(SymbExpr *a){
	SymbExpr *olda = _a;
	if (a) {
		_a = a->copy();
		_a->set_parent(this);
	} else
		_a = NULL;
	// in last, because a can be delete here, we must copy it before
	delete olda;
}

/**
 * Set the second child expression (b)
 * @param b a pointer to the new second child
 *
 * A *copy* of b will be set as second child.
 * If there was a second child in this expression, it will be delete,
 * but after the copy of b (so it's safe to set_b() a child expression
 * of this->b()).
*/
void SymbExpr::set_b(SymbExpr *b){
	SymbExpr *oldb = _b;
	if (b) {
		_b = b->copy();
		_b->set_parent(this);
	} else
		_b = NULL;
	// in last, because b can be delete here, we must copy it before
	delete oldb;
}

/**
 * @fn void SymbExpr::set_parent(SymbExpr *parent);
 * Set the reference to the parent of this
 * @param parent a pointer to the new parent.
 *
 * Warning: this do not remove the reference to this in the old parent.
 */

///
SymbExpr* SymbExpr::solidifyAddress(clp::State& clpState, bool dig) {
	if(_a) {
		_a = _a->solidifyAddress(clpState, dig);
		if(_a)
			_a->canonize();
	}
	if(_b) {
		_b = _b->solidifyAddress(clpState, dig);
		if(_b)
			_b->canonize();
	}
	if(!_a && !_b)
		return NULL;
	if(!_a)
		return _b;
	if(!_b)
		return _a;
	return this;
}


/**
 * @class SEConst
 *Constants
 */

/**
 * @fn SEConst::SEConst(V value, SymbExpr *parent=NULL);
 * Build a new Constant Symbolic Expression [K value]
 * @param value the value of the constant
 * @param parent a pointer to the parent
 */

/**
 * @fn SEConst::SEConst(const SEConst &expr): SymbExpr(expr);
 * Copy constructor
 * @param expr the expression to copy
 */

///
SEConst* SEConst::copy(void) {
	return new SEConst(_val);
}

///
SymbExpr& SEConst::operator=(const SEConst& expr) {
	_op = expr._op;
	_val = expr._val;
	return *this;
}

///
bool SEConst::operator==(const SymbExpr& expr) const {
	return (_op == expr.op() && _val == expr.val());
}

///
String SEConst::asString(const hard::Platform *pf) {
	if(_val.isConst())
		return (_ << "0x" << hex(_val.lower()));
	else if (_val.kind() == clp::ALL)
		return (_ << 'T');
	else {
		string temp;
		temp = temp << "(0x" << hex(_val.lower()) << ", 0x" << hex(_val.delta());
		if(_val.mtimes()!= clp::uintn_t(-1))
			temp = temp << ", 0x" << hex(_val.mtimes()) << ")";
		else
			temp = temp << ", inf)";
		return temp;
	}
}

///
void SEConst::canonize(void) {
}

///
SymbExpr* SEConst::solidifyAddress(clp::State& clpState, bool dig) {
	return new SEConst(_val, _parent);
}


/**
 * @class SEAddr
 * Memory reference
 * 
 * When _val is -1, this indicates that the SEAddr does not contain the constant address but a symbolic expression (_a != NULL).
 * otherwise _val is the constant address.
 */

/**
 * @fn SEAddr(V value, SymbExpr* a = NULL, SymbExpr* parent = NULL);
 * Build a new Memory Reference Symbolic Expression [@ value]
 * @param value the memory address
 * @param parent a pointer to the parent
 */

/**
 * @fn SEAddr::SEAddr(const SEAddr &expr)
 * Copy constructor
 * @param expr the expression to copy
 */

///
SEAddr* SEAddr::copy(void) {
	return new SEAddr(_val, _a?_a->copy():NULL);
}

///
SymbExpr& SEAddr::operator=(const SEAddr& expr) {
	_op = expr._op;
	_val = expr._val;
	return *this;
}

///
bool SEAddr::operator==(const SymbExpr& expr) const {
	return (_op == expr.op() && _val == expr.val());
}

///
String SEAddr::asString(const hard::Platform *pf) {
	if (_a) { // the address is an expression
		return (_ << "@" << _a->asString(pf));
	}
	else if(_val.isConst()) {
		return (_ << "@" << hex(_val.lower()));
	}
	else
		return (_ << "@(0x" << hex(_val.lower()) \
					<< ", 0x" << hex(_val.delta()) \
					<< ", 0x" << hex(_val.mtimes()) << ')');
}

///
void SEAddr::canonize(void) {
	// recursive call
	if (_a)
		_a->canonize();

	// when _val == -1, this tells that before _a is canonized, this SEAddr does not contain a solid address value
	// hence _a will be checked to see if the solid address, i.e. _a is a CONST, is obtained, in this case
	// _val will be assigned to _a's value and _a will be deleted.
	// otherwise, if _a is not canonized as a constant, this SEAddr will remain as a symbolic address
	if(_a && _a->op() == CONST && _a->val() == Value::top) {
		if (_parent->a() == this){
			// WILL DELETE this !
			SEConst* temp = new SEConst(Value::top);
			_parent->set_a(temp);
			delete temp;
			return;
		} else if (_parent->b() == this){
			// WILL DELETE this !
			SEConst* temp = new SEConst(Value::top);
			_parent->set_b(temp);
			delete temp;
			return;
		}
	}
	else if(_a && _a->op() == CONST) {
		_val = _a->val();
		delete _a;
		_a = NULL;
	}
}

///
Vector<Value> SEAddr::used_addr(void) {
	Vector<Value> vect;
	if(!_a)
		vect.add(_val);
	return vect;
}

///
SymbExpr* SEAddr::solidifyAddress(clp::State& clpState, bool dig) {
	if(_a) {
		SymbExpr* newA = _a->solidifyAddress(clpState, true);
		delete _a;
		_a = newA;
		if(!_a)
			return NULL;
		else
			_a->canonize();

		return copy();
	}
	return this; // nothing solidify
}


/**
 * @class SEReg
 * Register
 */

/**
 * @fn SEReg::SEReg(V value, SymbExpr *parent=NULL);
 * Build a new Register Symbolic Expression [R value]
 * @param value the identifier of the register (<0 for temp registers)
 * @param parent a pointer to the parent
 */

/**
 * @fn SEReg::SEReg(const SEReg &expr);
 * Copy constructor
 * @param expr the expression to copy
 */

///
SEReg* SEReg::copy(void) {
	return new SEReg(_val);
}

///
SymbExpr& SEReg::operator=(const SEReg& expr) {
	_op = expr._op;
	_val = expr._val;
	return *this;
}

///
bool SEReg::operator==(const SymbExpr& expr) const {
	return (_op == expr.op() && _val == expr.val());
}

///
String SEReg::asString(const hard::Platform *pf) {
	ASSERT(_val.isConst());
	if (_val >= 0) {
		if(pf)
			return pf->findReg(_val.lower())->name();
		else
			return (_ << 'r' << _val.lower());
	}
	else
		return (_ << 't' << - _val.lower());
}

///
void SEReg::canonize(void) {
}

///
Vector<Value> SEReg::used_reg(void) {
	Vector<Value> vect;
	vect.add(_val);
	return vect;
}

///
SymbExpr* SEReg::solidifyAddress(clp::State& clpState, bool dig) {
	ASSERT(_val.isConst());
	if(dig == false)
		return new SEReg(this->val(), this->parent());
	if(_val.lower() < 0)
		return new SEReg(this->val(), this->parent());

	clp::Value clpval = clpState.get(clp::Value(clp::REG, _val.lower()));
	if(clpval.isConst()) {
		return new SEConst(clpval, _parent);
	}
	else {
		return new SEReg(this->val(), this->parent());
	}
}


/**
 * @class SENeg
 * Negation
 */

/**
 * @fn SENeg::SENeg(SymbExpr *expr=NULL, SymbExpr *parent=NULL);
 * Build a new Negation Symbolic Expression [- a]
 * @param expr a pointer to the first child expression. This pointer
 *			will be used as is (no copy), so make sure you 'new' or
 *			'->copy()' the expression before.
 * @param parent a pointer to the parent
 */

/**
 * @fn SENeg::SENeg(const SENeg &expr);
 * Copy constructor
 * @param expr the expression to copy
 */

///
SENeg* SENeg::copy(void) {
	SymbExpr *newa = NULL;
	newa = _a->copy();
	return new SENeg(newa);
}

///
SymbExpr& SENeg::operator=(const SENeg& expr) {
	_op = expr._op;
	set_a(expr._a);
	return *this;
}

///
bool SENeg::operator==(const SymbExpr& expr) const {
	return (
		_op == expr.op() &&
		_a != NULL && expr.a() != NULL &&
		*_a == *(expr.a())
	);
}

///
String SENeg::asString(const hard::Platform *pf) {
	return (_ << "[-| " << _a->asString(pf) << ']');
}

///
void SENeg::canonize(void) {
	// recursive call
	if (_a)
		_a->canonize();

	/* In the next two case, we'll delete this, so we make sure we are
		referenced by a parent (and not created on the heap) */
	if(_parent == NULL)
		return;

	// [-, [K, <val>]] -> [K, eval(val * -1)]
	if (_a && _a->op() == CONST){
		if (_parent->a() == this){
			SEConst* temp = new SEConst(Value(0) - _a->val());
			_parent->set_a(temp); // WILL DELETE this !
			delete temp;
			return;
		} else if (_parent->b() == this){
			SEConst* temp = new SEConst(Value(0) - _a->val());
			_parent->set_b(temp); // WILL DELETE this !
			delete temp;
			return;
		}
	}
	// [-, [-, <expr>]] -> <expr>
	if(_a && _a->op() == NEG){
		if (_parent->a() == this){
			_parent->set_a(_a->a()); // WILL DELETE this !
			return;
		} else if (_parent->b() == this){
			_parent->set_b(_a->a()); // WILL DELETE this !
			return;
		}
	}
}

///
SymbExpr* SENeg::solidifyAddress(clp::State& clpState, bool dig) {
	if(_a) {
		SymbExpr* newA = _a->solidifyAddress(clpState, dig);
		if(newA != _a)
			delete _a;
		_a = newA;
		if(_a)
			_a->canonize();
		else
			return NULL;
	}

	SymbExpr* newThis = this->copy();
	newThis->set_parent(_parent);
	return newThis;
}



/**
 * @class SEAdd
 * Addition
 */

/**
 * @fn SEAdd::SEAdd(SymbExpr *a=NULL, SymbExpr *b=NULL, SymbExpr *parent=NULL);
 * Build a new Addition Symbolic Expression [+ a b]
 * @param a is a pointer to the first child expression. This pointer
 *			will be used as is (no copy), so make sure you 'new' or
 *			'->copy()' the expression before.
 * @param b is a pointer to the second child expression. This pointer
 *			will be used as is (no copy), so make sure you 'new' or
 *			'->copy()' the expression before.
 * @param parent a pointer to the parent
 */

/**
 * @fn SEAdd::SEAdd(const SEAdd &expr);
 * Copy constructor
 * @param expr the expression to copy
 */

///
SEAdd* SEAdd::copy(void) {
	return new SEAdd(_a->copy(), _b->copy());
}

///
SymbExpr& SEAdd::operator=(const SEAdd& expr) {
	_op = expr._op;
	set_a(expr._a);
	set_b(expr._b);
	return *this;
}

///
bool SEAdd::operator==(const SymbExpr& expr) const {
	return (
		_op == expr.op() &&
		_a != NULL && expr.a() != NULL &&
		*_a == *(expr.a()) &&
		_b != NULL && expr.b() != NULL &&
		*_b == *(expr.b())
	);
}

///
String SEAdd::asString(const hard::Platform *pf) {
	return (_ << "[+ " << _a->asString(pf) << ' ' << _b->asString(pf) << ']');
}

///
void SEAdd::canonize(void) {
	// recursive call
	if (_a)
		_a->canonize();
	if (_b)
		_b->canonize();

	// [+, [K, <val1>], [K, <val2>]] -> [K, eval(<val1> + <val2>)]
	// This case will replace this in _parent !
	if (_parent && _a && _a->op() == CONST && _b && _b->op() == CONST){
		if (_parent->a() == this){
			// WILL DELETE this !
			SEConst* temp = new SEConst(_a->val() + _b->val());
			_parent->set_a(temp);
			delete temp;
			return;
		} else if (_parent->b() == this){
			// WILL DELETE this !
			SEConst* temp = new SEConst(_a->val() + _b->val());
			_parent->set_b(temp);
			delete temp;
			return;
		}
	}

	// [+, [K, <val1>], <expr1>] -> [+, <expr1>, [K, <val1>]]
	if (_a && _a->op() == CONST && _b){
		SymbExpr *expr = _b;
		_b = _a;
		_a = expr;
	}

	// [+, [+, V, <expr1>], <expr2>] -> [+, V, canonize([+, <expr1>, <expr2>])]
	// with V either a SEReg or a SEAddr
	if (_a && _a->op() == ADD && _a->a() &&
			(_a->a()->op() == REG || _a->a()->op() == ADDR)){
		SEAdd *newb = new SEAdd(_a->b()->copy(), _b, _parent);
		newb->canonize();
		_b = newb;
		set_a(_a->a());
	}
}

///
SymbExpr* SEAdd::solidifyAddress(clp::State& clpState, bool dig) {
	if(_a) {
		SymbExpr* newA = _a->solidifyAddress(clpState, dig);
		if(newA != _a)
			delete _a;
		_a = newA;
		if(_a)
			_a->canonize();
		else
			return NULL;
	}

	if(_b) {
		SymbExpr* newB = _b->solidifyAddress(clpState, dig);
		if(newB != _b)
			delete _b;
		_b = newB;
		if(_b)
			_b->canonize();
		else
			return NULL;
	}

	SymbExpr* newThis = this->copy();
	newThis->set_parent(_parent);
	return newThis;
}

/**
 * @class SECmp
 * Compare symbolic expression
 * This class define three type of compare expression:
 * condition (if): SECmp(operator, expression)
 * undetermined compare: SECmp(CMP, exp1, exp2)
 * determined compare: SECmp(operator, exp1, exp2)
 */

/**
 * @fn SECmp::SECmp(op_t op, SymbExpr *a=NULL, SymbExpr *b=NULL, SymbExpr *parent=NULL);
 * Build a new Addition Symbolic Expression]
 * @param a is a pointer to the first child expression. This pointer
 *			will be used as is (no copy), so make sure you 'new' or
 *			'->copy()' the expression before.
 * @param b is a pointer to the second child expression. This pointer
 *			will be used as is (no copy), so make sure you 'new' or
 *			'->copy()' the expression before.
 * @param parent a pointer to the parent
 *
 * This class can reprensts three forms of expressions:
 * (oplog is in [LE, LT, GE, GT, EQ, NE])
 *	* [oplog a] for a condition (sem instr 'if')
 *	* [CMP a b] for an undetermined compare (sem instr 'cmp')
 *	* [oplog a b] for a determined compare (result of canonize)
 */

/**
 * @fn SECmp::SECmp(const SECmp &expr);
 * Copy constructor
 * @param expr the expression to copy
 */

///
SECmp* SECmp::copy(void) {
	if (_b == NULL)
		return new SECmp(_op, _a->copy());
	else
		return new SECmp(_op, _a->copy(), _b->copy());
}

///
SymbExpr& SECmp::operator=(const SECmp& expr) {
	_op = expr._op;
	set_a(expr._a);
	set_b(expr._b);
	return *this;
}

///
bool SECmp::operator==(const SymbExpr& expr) const {
	if ((_op != expr.op()) || _a == NULL || expr.a() == NULL ||  (*_a != *(expr.a())))
		return false;
	if (_b == NULL && expr.b() == NULL)
		return true;
	if (_b == NULL || expr.b() == NULL)
		return false;
	else
		return (*_b == *(expr.b()));
}

///
String SECmp::asString(const hard::Platform *pf) {
	String s = "[";
	switch(_op){
	case OR:
		return "OR]";
	case CMP:
		s = s << "cmp";
		break;
	case CMPU:
		s = s << "cmpu";
		break;
	case LE:
		s = s << "<=";
		break;
	case LT:
		s = s << '<';
		break;
	case GE:
		s = s << ">=";
		break;
	case GT:
		s = s << '>';
		break;
	case EQ:
		s = s << '=';
		break;
	case NE:
		s = s << "/=";
		break;
	case ULE:
		s = s << "u<=";
		break;
	case ULT:
		s = s << "u<";
		break;
	case UGE:
		s = s << "u>=";
		break;
	case UGT:
		s = s << "u>";
		break;
	default:
		break;
	}
	s = s << ' ' << _a->asString(pf);
	if (_b != NULL)
		s = s << ' ' << _b->asString(pf);
	s = s << ']';
	return s;
}

///
void SECmp::canonize(void) {
	// recursive call
	if (_a)
		_a->canonize();
	if (_b)
		_b->canonize();

	if (_op == CMP || _op == CMPU)
		return;		// we need a determinated CMP for further canonization

	bool cancont;

	do{
		cancont=false;

		// [<log_op>, [cmp, <expr1>, <expr2>]] -> [<log_op>, <expr1>, expr2>]
		if (_a && _a->op() == CMP && _b == NULL){
			set_b(_a->b()); // we must set b first, because we'll erase _a
			set_a(_a->a());
			cancont = true;
		}
		// The same for unsigned compare
		if (_a && _a->op() == CMPU && _b == NULL){
			/* change the operator into unsigned form */
			switch(_op){
			case LE:
				_op = ULE;
				break;
			case LT:
				_op = ULT;
				break;
			case GE:
				_op = UGE;
				break;
			case GT:
				_op = UGT;
				break;
			default:
				break;
			}
			set_b(_a->b()); // we must set b first, because we'll erase _a
			set_a(_a->a());
			cancont = true;
		}

		// [<log_op>, [K, <valeur>], <expr>] && <expr> != const
		// -> [reverse(<log_op>), <expr>, [K, <valeur>]]
		if (_a && _b && _a->op() == CONST && _b->op() != CONST){
			_op = reverse(_op);
			SymbExpr *expr = _b;
			_b = _a;
			_a = expr;
			cancont = true;
		}

		// [<log_op>, [+, <expr0>, <expr1>], <expr2>]
		// -> [<log_op>, <expr0>, canonize([+, <expr2>, [-, <expr1>]])]
		if (_a && _b && _a->op() == ADD && _a->a() && _a->b()){
			SEAdd newb = SEAdd(_b->copy(), new SENeg(_a->b()->copy()));
			set_b(&newb);
			set_a(_a->a());
			_b->canonize();
			cancont = true;
		}

		// [<log_op>, [-, <expr0>], expr1]
		// -> [reverse(<log_op>), <expr0>, canonize([-, <expr1>])]
		if (_a && _b && _a->op() == NEG && _a->a()){
			_op = reverse(_op);
			set_a(_a->a());
			SymbExpr *newb = new SENeg(_b->copy());
			set_b(newb);
			delete newb;
			_b->canonize();
			cancont = true;
		}
	}while(cancont);
}

///
SECmp* SECmp::logicalNot(void) {
	/* not the logical operator */
	op_t newop;
	switch(_op){
	case LE:
		newop = GT;
		break;
	case LT:
		newop = GE;
		break;
	case GE:
		newop = LT;
		break;
	case GT:
		newop = LE;
		break;
	case EQ:
		newop = NE;
		break;
	case NE:
		newop = EQ;
		break;
	case ULE:
		newop = UGT;
		break;
	case ULT:
		newop = UGE;
		break;
	case UGE:
		newop = ULT;
		break;
	case UGT:
		newop = ULE;
		break;
	case OR:
		newop = NONE;
		break;
	default:
		// others (invalid) are returned unchanged
		newop = _op;
		break;
	}
	SECmp *notse = new SECmp(newop, _a->copy(), _b->copy());
	return notse;
}

///
bool SECmp::isValid(void) {
	if(_a->op() != _b->op())
		return true; // unknown, so guess it is true

	if((_a->op() == CONST) && (_b->op() == CONST)) { // then we can evaluate directly
		switch(_op){
		case LE:
			return (_a->val().lower() <= _b->val().lower());
		case LT:
			return (_a->val().lower() < _b->val().lower());
		case GE:
			return (_a->val().lower() >= _b->val().lower());
		case GT:
			return (_a->val().lower() > _b->val().lower());
		case EQ:
			return (*_a == *_b);
		case NE:
			return (*_a != *_b);
		case ULE: // FIXME: to implement
			return ((clp::uintn_t)(_a->val().lower()) <= (clp::uintn_t)(_b->val().lower()));
		case ULT:
			return ((clp::uintn_t)(_a->val().lower()) < (clp::uintn_t)(_b->val().lower()));
		case UGE:
			return ((clp::uintn_t)(_a->val().lower()) >= (clp::uintn_t)(_b->val().lower()));
		case UGT:
			return ((clp::uintn_t)(_a->val().lower()) > (clp::uintn_t)(_b->val().lower()));
		default:
			elm::cerr << "SECmp::isValid(): WARNING! unable to determine the validity for " << this->asString() << io::endl;
			return true;
		} // end of the switch
	} // end of CONST comparisons
	else if((_a->op() == REG) && (_b->op() == REG)) // always true for relations with registers
		return true;
	else if((_a->op() == ADDR) && (_b->op() == ADDR)) // always true for relations with registers
		return true;
	else {
		switch(_op){
		case EQ:
			return (*_a == *_b);
		case NE:
			return (*_a != *_b);
		case LT:
		case GE:
		case GT:
		case LE:
		case ULE:
		case ULT:
		case UGE:
		case UGT:
		default:
			elm::cerr << "SECmp::isValid(): WARNING! unable to determine the validity for " << this->asString() << io::endl;
			return true;
		}
	}
	return true;
}


/**
 * @class FilterBuilder
 */

/**
 * Build a filter and install them on the current BB.
 * @param _bb	BB to work on.
 */
FilterBuilder::FilterBuilder(BasicBlock *_bb, clp::Problem& problem): bb(_bb) {
	getFilters();
}

/**
 * Find filters that apply on the basic block
 * Two properties are sets:
 *		REG_FILTERS for filters on registers
 *		ADDR_FILTERS for filters on memory addresses
 */
void FilterBuilder::getFilters(void) {

	// collect bundles and identify branch
	Vector<BasicBlock::Bundle> bundles;
	BasicBlock::Bundle branchBundle;
	bool branch_set = false;
	for(auto bbbi: bb->bundles()) {

		// find the branch bundle
		if(bbbi.kind() & Inst::IS_CONTROL) {
			for(auto bi: bbbi.insts()) {
				if(bi->isControl())
					if(bi->isConditional()) {
						branchBundle = bbbi;
						branch_set = true;
					}
			}
		}

		// add every bundle besides the branching bundle
		if(	!branch_set
		|| branchBundle.address() != bbbi.address())
			bundles.add(bbbi);
	}

	// create the filters, starting with the bundle that contains the branch
	ASSERT(branch_set);
	iterateBranchPaths(branchBundle, bundles);

	// attach filters to BB
	REG_FILTERS(bb) = reg_filters;
	ADDR_FILTERS(bb) = addr_filters;

	// attach filters to edges
	for(BasicBlock::EdgeIter bbei=bb->outs(); bbei(); bbei++) {
		if(bbei->isTaken()) {
			REG_FILTERS(*bbei) = reg_filters;
			ADDR_FILTERS(*bbei) = addr_filters;
		}
		else if(bbei->isNotTaken()) {
			REG_FILTERS(*bbei) = reg_filters_not;
			ADDR_FILTERS(*bbei) = addr_filters_not;
		}
	}
}

/**
 * Iterate on all semantics execution paths and call makeFilters().
 * @param branchBundle	The bundle which contains the brach sem inst as the starting point of making the filters.
 * @param bundles		The list of bundles to carry out filter making for each bundles in the processing BB after the branchBundle.
	*/
void FilterBuilder::iterateBranchPaths(const BasicBlock::Bundle& branchBundle, const Vector<BasicBlock::Bundle>& bundles) {

	// useful structure
	typedef struct path_t {
		inline path_t(void): i(0), n(0), b(false) { }
		inline path_t(int _i, int _n, bool _b): i(_i), n(_n), b(_b) { }
		int i;	// i in istack
		int n;	// n in block
		bool b;	// branch found
	} path_t;

	bool first = true;
	bool first_not = true;
	Vector<path_t> pstack;
	sem::Block semInstStack, block;
	branchBundle.semInsts(block);

	pstack.push(path_t(0, 0, false));
	while(pstack) {
		path_t path = pstack.pop();
		semInstStack.setLength(path.i); // ignore the rest of the stack after the ith semantic instruction
		if(path.i != 0) { // besides the first pop, we reverse the condition as each popping is the "alternative path"
			semInstStack[path.i - 1]._d = reverseCond(sem::cond_t(semInstStack[path.i - 1].d()));
		}
		
		// Traverse instructions of the path. The path.n is the current index of the semantic instruction being inspected.
		while(path.n < block.count()) {
			if(block[path.n].op == sem::IF)
			{
				if(block[path.n].cond() == sem::NO_COND)
					path.n += block[path.n].b();
				else {
					semInstStack.push(block[path.n]);
					pstack.push(path_t(semInstStack.length(), path.n + block[path.n].b() + 1, path.b)); // add the alternative path to the work list
				}
			}
			else if(block[path.n].op == sem::CONT)
			{
				semInstStack.push(block[path.n]);
				break;
			}
			else if(block[path.n].op == sem::BRANCH)
			{
				semInstStack.push(block[path.n]);
				path.b = true;
			}
			else
				semInstStack.push(block[path.n]);

			path.n++;
		}

		// process the path
		if(path.b) {
			if(first)
				first = false;
			else {
				reg_filters.add(new SECmp(SymbExpr::OR));
				addr_filters.add(new SECmp(SymbExpr::OR));
			}
			// we have the control instruction inst, and its previous semantic instructions, we can
			// use the previous sem insts to build an expression the condition of the branch
			// the filter is making in the reverse order, so that the expression is reduced and enriched by the previous sem inst.
			// We first obtain the expression from the branch instruction. The semInstStack only contains the semantic instructions of the branchInst
			SECmp *se = makeFilters(NULL, branchBundle, semInstStack, true);
			// Now we use the just-created se to explore more conditions by following up the previous sem insts.
			addFilters(se, bundles);

			reg_filters.addAll(curr_reg_filters);
			addr_filters.addAll(curr_addr_filters);
			curr_reg_filters.clear();
			curr_addr_filters.clear();
			curr_known_reg.clear();
			curr_known_addr.clear();
		}
		else {
			if(first_not)
				first_not = false;
			else {
				reg_filters_not.add(new SECmp(SymbExpr::OR));
				addr_filters_not.add(new SECmp(SymbExpr::OR));
			}
			SECmp *se = makeFilters(NULL, branchBundle, semInstStack, true);
			addFilters(se, bundles);

			reg_filters_not.addAll(curr_reg_filters);
			addr_filters_not.addAll(curr_addr_filters);
			curr_reg_filters.clear();
			curr_addr_filters.clear();
			curr_known_reg.clear();
			curr_known_addr.clear();
		}
	} // end of pstack}
}

///
sem::cond_t FilterBuilder::reverseCond(sem::cond_t cond) {
	switch(cond) {
	case sem::NO_COND:	return sem::NO_COND;
	case sem::EQ: 		return sem::NE;
	case sem::LT:		return sem::GE;
	case sem::LE:		return sem::GT;
	case sem::GE:		return sem::LT;
	case sem::GT:		return sem::LE;
	case sem::ANY_COND:	return sem::ANY_COND;
	case sem::NE:		return sem::EQ;
	case sem::ULT:		return sem::UGE;
	case sem::ULE:		return sem::UGT;
	case sem::UGE:		return sem::ULT;
	case sem::UGT:		return sem::ULE;
	default:			ASSERT(false); return sem::NO_COND;
	}
}


///
SECmp *getFilterForReg(
	SECmp *se,
	Value reg,*
	clp::ClpStatePack &pack,
	Inst *i,
	int sem,
	Vector<V> &used_reg,
	Vector<V> &used_addr
){
	/* FIXME : This could be otptimized: we do a CLP analysis from the
		beginning of the BB each time we replace a register by its value */
	clp::State state = pack.state_before(i->address(), sem);

	ASSERT(reg.isConst());

	// replace other registers
	for (int i=0; i < used_reg.length(); i++){
		ASSERT(used_reg[i].isConst());
		if(used_reg[i].lower() != reg.lower()){
			// get the actual value of used_reg[i]
			clp::Value clpval = state.get(clp::Value(clp::REG, used_reg[i].lower()));
			SEConst *val = new SEConst(clpval);
			SEReg *r = new SEReg(used_reg[i]);
			se->replace(r, val);
			delete r;
			delete val;
		}
	}

	// replace other memory refs
	for (int i=0; i < used_addr.length(); i++){
		ASSERT(used_addr[i].isConst());
		// get the actual value of used_addr[i]
		clp::Value clpval = state.get(used_addr[i]);
		SEConst *val = new SEConst(clpval);
		SEAddr *a = new SEAddr(used_addr[i]);
		se->replace(a, val);
		delete a;
		delete val;
	}

	// canonize
	se->canonize();
	// check if we have a filter
	if (se->op() > CMPU && se->a() && se->a()->op() == REG && se->b() && se->b()->op() == CONST /*&& (se->b()->val() != V::all)*/) {
						// a exists   // a is a REG         // b exits   // b is a CONST
		if (se->b()->val() == V::top) {
			return NULL;
		}
		else
			return se;
	}
	else{
		TRACEGF(cerr << "Bad filter: " << se->asString() << "\n";)
		return NULL;
	}
}

SECmp *getFilterForAddr(SECmp *se, V addr, clp::ClpStatePack &pack, const BasicBlock::Bundle &i, int sem, Vector<V> &used_reg, Vector<V> &used_addr){
	/* FIXME: this could be otptimized: we do a CLP analysis from the
		beginning of the BB each time we replace an address by its value */
	clp::State state = pack.state_before(i.address(), sem);
	ASSERT(addr.isConst());
	while(1) {
		Vector<V> used_reg = se->used_reg();
		Vector<V> used_addr = se->used_addr();

		// replace other registers
		for (int i=0; i < used_reg.length(); i++){
			ASSERT(used_reg[i].isConst());
			// get the actual value of used_reg[i]
			clp::Value clpval = state.get(clp::Value(clp::REG, used_reg[i].lower()));
			SEConst *val = new SEConst(clpval);
			SEReg *r = new SEReg(used_reg[i]);
			se->replace(r, val);
			delete r;
			delete val;
		}

		// replace other memory refs
		for (int i=0; i < used_addr.length(); i++){
			ASSERT(used_addr[i].isConst());
			if (used_addr[i] != addr){
				// get the actual value of used_addr[i]
				clp::Value clpval = state.get(used_addr[i]);
				SEConst *val = new SEConst(clpval);
				SEAddr *a = new SEAddr(used_addr[i]);
				se->replace(a, val);
				delete a;
				delete val;
			}
		}

		SECmp* temp = se->copy();
		// canonize
		se->canonize();
		// check if we have a filter
		if(*temp == *se) {
			delete temp;
			break;
		}
		else {
			delete temp;
		}
	}

	if (se->op() > CMPU && se->a() && se->a()->op() == ADDR && se->b() && se->b()->op() == CONST /*&& (se->b()->val() != V::all)*/) {
		if (se->b()->val() == V::top)
			return NULL;
		if (!se->a()->val().isConst())
			return NULL;
		if (se->b()->val().isInf())
			return NULL;

		return se;
	}
	else{
		TRACEGF(cerr << "Bad filter: " << se->asString() << "\n";)
		return NULL;
	}
}

	
///
SECmp *FilterBuilder::makeFilters(
	SECmp *se_orig,
	const BasicBlock::Bundle& currentBundle,
	sem::Block& bb,
	bool branch
) {
	typedef Vector<SECmp *> filters_t;
	typedef Vector<Value> regs_t;
	typedef Vector<Value> addrs_t;

	Vector<filters_t> temp_reg_filters;
	Vector<filters_t> temp_addr_filters;
	Vector<regs_t> temp_known_reg;
	Vector<addrs_t> temp_known_addr;
	SECmp *seToReturn = 0;

	Vector<sem::Block> semBlocks; // each path is associated with a sem::Block
	if(branch)
		semBlocks.add(bb);
	else
		prepareSemBlockPaths(semBlocks, bb);

	for(int bi = 0; bi < semBlocks.count(); bi++) {
		temp_reg_filters.add(filters_t());
		temp_addr_filters.add(filters_t());
		temp_known_reg.add(regs_t());
		temp_known_addr.add(addrs_t());
	}

	SECmp *se = 0;
	bool pathFailed = false;

	for(int bi = 0; bi < semBlocks.count(); bi++) {
		// prepare the se for this path
		if(se_orig) // make a copy of se_orig
			se = se_orig->copy();
		else
			se = 0;

		sem::Block& b = semBlocks[bi];
		// traverse reversely in the given semantic instruction block
		for(int pc=b.length() - 1; pc >= 0; pc--){
			sem::inst& i = b[pc];
			// build the matching SE
			switch(i.op) {

			case sem::ASSUME:
			case sem::IF: { // If inst is a if:
					// create a new symbexpr
					SymbExpr::op_t log_op = SymbExpr::NONE;
					switch(i.cond()){
					case sem::LE: 		log_op = SymbExpr::LE; break;
					case sem::LT: 		log_op = SymbExpr::LT; break;
					case sem::GE: 		log_op = SymbExpr::GE; break;
					case sem::GT: 		log_op = SymbExpr::GT; break;
					case sem::EQ: 		log_op = SymbExpr::EQ; break;
					case sem::NE:		log_op = SymbExpr::NE; break;
					case sem::ULE: 		log_op = SymbExpr::ULE; break;
					case sem::ULT: 		log_op = SymbExpr::ULT; break;
					case sem::UGE: 		log_op = SymbExpr::UGE; break;
					case sem::UGT: 		log_op = SymbExpr::UGT; break;
					case sem::ANY_COND:	log_op = SymbExpr::NONE; break;
					default:			ASSERTP(false, "unsupported condition " << i.cond() << " at " << currentBundle.address()); break;
					}
					if(log_op != SymbExpr::NONE) {
						temp_known_reg[bi].clear();
						temp_known_addr[bi].clear();
						// FIXME
						// this means if there are more than one IF semantic instructions to process, the one processed first will be removed from the earth
						if(se)
							delete se;

						se = new SECmp(log_op, new SEReg(i.a()));
					}
				}
				break;

			case sem::SET:
				if(se) {
					SEReg *rd = new SEReg(i.d());
					SEReg *rs = new SEReg(i.a());
					se->replace(rd, rs);
					delete rd;
					delete rs;
				}
				break;

			// If inst is another instruction: replace
			case sem::LOAD:
				if (se){
					SEReg *rd = new SEReg(i.d());
					// get the address of the register i.a()
					//clp::State state = pack.state_after(cur_inst->address(), pc);
					clp::State state = pack.state_after(currentBundle.address(), pc);
					if(i.a() >= 0) {
						clp::Value val = state.get(clp::Value(clp::REG, i.a()));
						if (val != clp::Value::all){
							if(!val.isConst()){
								cerr << "WARNING: unconst address: " << val << endl;
								// if val is a set, we cannot insert the memory
								// reference in the filter
								// TODO: maybe we should 'fork' the filter?
								// For the moment, if the load concern this expr
								// we set the se to NULL, to
								// invalidate the register i.a()
								Vector<Value> used_reg = se->used_reg();
								for(int i = 0; i < used_reg.length(); i++){
									if(used_reg[i] == rd->val()){
										delete se;
										se = NULL;
										break;
									}
								}
							} else {
								SEAddr *a = new SEAddr(val.lower());
								se->replace(rd, a);
								delete a;
							}
						}

						else {
							SEAddr *a = new SEAddr(-1 ,new SEReg(i.a()));
							se->replace(rd, a);
							delete a;
						}
					} // if register to specify the address is not a temp register
					else {
						SEAddr *a = new SEAddr(-1 ,new SEReg(i.a()));
						se->replace(rd, a);
						delete a;
					}

					delete rd;
				}
				break;

			case sem::CMPU:
				if (se){
					SEReg *rd = new SEReg(i.d());
					SECmp *cmp = new SECmp(SymbExpr::CMPU, new SEReg(i.a()), new SEReg(i.b()));
					se->replace(rd, cmp);
					delete rd;
					delete cmp;
				}
				break;

			case sem::CMP:
				if (se){
					SEReg *rd = new SEReg(i.d());
					SECmp *cmp = new SECmp(SymbExpr::CMP, new SEReg(i.a()), new SEReg(i.b()));
					se->replace(rd, cmp);
					delete rd;
					delete cmp;
				}
				break;

			case sem::SETI:
				if (se){
					SEReg *rd = new SEReg(i.d());
					SEConst *c = new SEConst(i.a());
					se->replace(rd, c);
					delete rd;
					delete c;
				}
				break;

			case sem::ADD:
				if (se){
					SEReg *rd = new SEReg(i.d());
					SEAdd *add = new SEAdd(new SEReg(i.a()), new SEReg(i.b()));
					se->replace(rd, add);
					delete rd;
					delete add;
				}
				break;

			case sem::SUB:
				if (se){
					SEReg *rd = new SEReg(i.d());
					SEAdd *sub = new SEAdd(new SEReg(i.a()), new SENeg(new SEReg(i.b())));
					se->replace(rd, sub);
					delete rd;
					delete sub;
				}
				break;

			case sem::SCRATCH:
				if (se){
					SEReg *rd = new SEReg(i.d());
					// if the register rd is used in the expression, we set
					// se to NULL: we cannot find any further filter where
					// rd is implied.
					Vector<Value> used_reg = se->used_reg();
					for(int i = 0; i < used_reg.length(); i++){
						if(used_reg[i] == rd->val()){
							delete se;
							se = NULL;
							break;
						}
					}
					delete rd;
				}
				break;

			default:
				// wipe out the expression as the default behavior
				delete se;
				se = NULL;
				break;

			} // end of switch

			if(se) {
				se->canonize();
				// find filters...
				// This is carried out by looking at the expression. If the expression does not contain any temporary registers
				// then that means the expression is fully resolved.
				if (se->op() > SymbExpr::CMPU && se->a() && se->b()){

					SymbExpr* se2 = se->copy();
					clp::State state = pack.state_before(currentBundle.address(), pc);
					se2 = se2->solidifyAddress(state, false);

					Vector<Value> used_reg = se->used_reg();
					Vector<Value> used_addr;
					if(se2)
						used_addr = se2->used_addr();
					bool has_tmp = false;
					for(int i = 0; i < used_reg.length(); i++)
						if (! (used_reg[i] >= 0))
							has_tmp = true;
					if (!has_tmp){
						// check if there is a violation on the expression
						if (se->op() > SymbExpr::CMPU && se->a() && se->b()) {
							if(!se->isValid()) {
								pathFailed = true;
								break;
							}
						}
						// Reach here when the symbolic expression can be used to create a filter.
						// for each new register
						for(int i = 0; i < used_reg.length(); i++) {
							if(!curr_known_reg.contains(used_reg[i]) && !temp_known_reg[bi].contains(used_reg[i])) { // one register can only be added once
								// get the filter
								SECmp *newfilter = getFilterForReg(se->copy(), used_reg[i], pack, currentBundle.first(), pc, used_reg, used_addr);
								if (newfilter){
									temp_reg_filters[bi].add(newfilter);
									temp_known_reg[bi].add(used_reg[i]);
								}
							}
						} // end of processing current register filters

						// for each new addr
						for(int i = 0; i < used_addr.length(); i++) {
							if(!curr_known_addr.contains(used_addr[i]) && !temp_known_addr[bi].contains(used_addr[i])) { // one memory address can be only added once
								// get the filter
								SECmp *newfilter = getFilterForAddr(se->copy(), used_addr[i], pack, currentBundle, pc, used_reg, used_addr);
								if (newfilter){
									temp_addr_filters[bi].add(newfilter);
									temp_known_addr[bi].add(used_addr[i]);
								}
							}
						} // end of processing current address filters
					} // end of no temp register involved
				} // end of CMP
			} // end if(se)
		} // end of for(int pc=b.length() - 1; pc >= 0; pc--)

		if(!pathFailed) { // if the path is valid
			if(seToReturn) { // if the other path is also valid, we clear the filter to prevent confusions
				ASSERT(se);
				delete se;
				seToReturn = 0;
				for(int bix = 0; bix < semBlocks.count(); bix++) {
					temp_known_reg[bix].clear();
					temp_reg_filters[bix].clear();
					temp_known_addr[bix].clear();
					temp_addr_filters[bix].clear();
				}
				break;
			}
			else
				seToReturn = se;
		}
		pathFailed = false; // reset the flag
		// now we take another path, if there is any
	} // end of for(int bi = 0; bi < semBlocks.count(); bi++) // for each path within sem block


	// now decide which filters to add
	regs_t temp_known_reg_all, temp_known_reg_repeat;
	// first find out what are the repeat registers
	for(int bi = 0; bi < semBlocks.count(); bi++)
		for (int bii = 0; bii < temp_known_reg[bi].count(); bii++)
			if(temp_known_reg_all.contains(temp_known_reg[bi][bii]))
				temp_known_reg_repeat.add(temp_known_reg[bi][bii]);
			else
				temp_known_reg_all.add(temp_known_reg[bi][bii]);

	// add the filters whose associated register is not in the repeat registers
	for(int bi = 0; bi < semBlocks.count(); bi++)
		for(int bii = 0; bii < temp_reg_filters[bi].count(); bii++)
			if(!temp_known_reg_repeat.contains(temp_reg_filters[bi][bii]->a()->val())) {
				curr_reg_filters.add(temp_reg_filters[bi][bii]);
				curr_known_reg.add(temp_reg_filters[bi][bii]->a()->val());
			}


	addrs_t temp_known_addr_all, temp_known_addr_repeat;
	// first find out what are the repeat addr
	for(int bi = 0; bi < semBlocks.count(); bi++)
		for (int bii = 0; bii < temp_known_addr[bi].count(); bii++)
			if(temp_known_addr_all.contains(temp_known_addr[bi][bii]))
				temp_known_addr_repeat.add(temp_known_addr[bi][bii]);
			else
				temp_known_addr_all.add(temp_known_addr[bi][bii]);

	// add the filters whose associated addr is not in the repeat addr
	for(int bi = 0; bi < semBlocks.count(); bi++)
		for(int bii = 0; bii < temp_addr_filters[bi].count(); bii++)
			if(!temp_known_addr_repeat.contains(temp_addr_filters[bi][bii]->a()->val())) {
				int index = curr_known_addr.indexOf(temp_addr_filters[bi][bii]->a()->val(), 0);
				if(!((index != -1) && (curr_addr_filters[index]->b()->val() == temp_addr_filters[bi][bii]->b()->val()))) {
					curr_addr_filters.add(temp_addr_filters[bi][bii]);
					curr_known_addr.add(temp_addr_filters[bi][bii]->a()->val());
				}
			}

	return seToReturn;
}

/**
 * Add the filters for the current instruction list (taken backward).
 * makeFilters is called for every instructions in the insts
 * @param se		Current conditional branch comparison.
 * @param insts		Instructions of the block.
 */
void FilterBuilder::addFilters(SECmp *se, const Vector<BasicBlock::Bundle>& bundles) {
	sem::Block block;
	for(int i = bundles.count() - 1; i >= 0; i--) {
		block.clear();
		bundles[i].semInsts(block);
		se = makeFilters(se, bundles[i], block, false);
	}
	delete se;
}

///
void FilterBuilder::prepareSemBlockPaths(Vector<sem::Block>& semBlocks, const sem::Block& b) {
	// TODO
}


/**
	* Apply a filter on the value
	* @param v the CLP to be filtred
	* @param cmp_op compare operator
	* @param f CLP to filter with
*/
void applyFilter(Value &v, SymbExpr::op_t cmp_op, Value f) {
	// TODO
}

Identifier<Vector<SECmp *> > REG_FILTERS("");
Identifier<Vector<SECmp *> > ADDR_FILTERS("");

} }	// otawa::clp
