/*
 *	otawa::clp::Value class implementation
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

#include <otawa/clp/Value.h>

namespace otawa { namespace clp {

static const int ARITHMETIC_THRESHOLD = 1024;

/**
 * @typedef kind_t
 * Allowed types for values: NON, REG, VAL or ALL.
 */

/**
 * @var NONE
 * Represents nothing.
 */

/**
 * @var REG
 * It is only used for addresses, and represents a register.
 */

/**
 * @var VAL
 * Represents some values (either a constant or an interval).
 */

/**
 * @var ALL
 * This is the Top element.
 */

/**
 * @var CMP
 * This represents the result of a comparison. _base is the number of the first
 * compared register. _delta is the number of the second compared register.
 */


/**
 * Return positive GCD of two unsigned integers.
 * @param a	First integer.
 * @param b	Second integer.
 * @return	GCD(a, b).
 */
static uintn_t ugcd(uintn_t a, uintn_t b) {
	if (a == 0)
		return b;
	else if (b == 0)
		return a;
	else
		return ugcd(b, a % b);
}


/**
 * Return the GCD (Greatest Common Divisor) of two long integers.
 * Result is negative if both a and b are negative, positive else.
 * @param a		First value.
 * @param b		Second value.
 * @return		Negative GCD(a, b) if a < 0 and b < 0, positive GCD else.
 */
static intn_t gcd(intn_t a, intn_t b){
	if(a > 0) {
		if(b > 0)
			return ugcd(a, b);
		else
			return ugcd(a, -b);
	}
	else {
		if(b > 0)
			return ugcd(-a, b);
		else
			return -ugcd(-a, -b);
	}
}


/**
 * Return the lcm of two long integers
*/
inline long lcm(long a, long b){
	return elm::abs(a * b) / gcd(a, b);
}

/**
 * Return the min with a signed comparison
*/
inline intn_t min(intn_t a, intn_t b){
	if ( a < b)
		return a;
	else
		return b;
}

/**
 * Return the max with a signed comparison
*/
inline intn_t max(intn_t a, intn_t b){
	if ( a > b)
		return a;
	else
		return b;
}

/**
 * Return the min with an unsigned comparison
*/
inline uintn_t umin(uintn_t a, uintn_t b){
	if (a < b)
		return a;
	else
		return b;
}

/**
 * Return the max with an usigned comparison
*/
inline uintn_t umax(uintn_t a, uintn_t b){
	if ( a > b)
		return a;
	else
		return b;
}


/**
 * @class Value
 * Represents a CLP value, i.e.,
 * a set of values represented by a Circular Linear Progression.
 * Values are defined by a 3-tuple (lower, delta, mtimes) for 32bits numbers.
 * A constant integer k is represented by (k, 0, 0)
 * The Top element is represented by (0, 1, 2^32 - 1)
 * @ingroup clp
 */

/**
 * @fn @fn Value(kind_t kind, intn_t lower, intn_t delta, uintn_t mtimes)
 * Constructor for a new CLP
 *	@param kind the kind of the CLP
 *	@param lower the lower bound
 *	@param delta the delta
 *	@param mtimes the number of time the delta is applied
 */


/**
 * @fn Value(const int val)
 * Singleton constructor
 *	@param val the single integer element of the CLP
 *	@return the CLP (val, 0, 0)
 */

/**
 * Forms two CLP values which are the components of [base value, the value just before overflow]
 * and [the value just after overflow, upper bound of this part]. Refer to the beginning of the
 * section 6 of [Sen et Srikant, 2007].
 * @param p the CLP value contains [base value, the value just before overflow]
 * @param q the CLP value contains [the value just after overflow, upper bound of this part]
 */
void Value::PQValue(Value &p, Value &q) {
	ASSERT(_delta >= 0);
	intn_t l, u;
	l = _base;
	u = _base + _delta * _mtimes;
	if(l <= u) {
		p = *this;
		q = Value::none;
	}
	else {
		// calculate the P value
		// just to be careful with sign and unsigned ops, so use int64 and break the operation apart......
		t::int64 maxPm = MAXn;
		maxPm = maxPm - l;
		maxPm = maxPm / _delta;
		intn_t maxPd = (maxPm == 0)? 0 : _delta;
		p.set(VAL, l, maxPd, maxPm);

		// calculate the Q value
		t::int64 minQm = u;
		minQm = minQm - MINn;
		minQm = minQm / _delta;
		intn_t minQ = u - _delta * minQm;
		intn_t minQd = (minQm == 0)? 0 : _delta;
		q.set(VAL, minQ, minQd, minQm);
	}
}

/**
 * @fn Value Value::operator+(const Value& val) const;
 * Overload of the addition operator.
 */

/**
 * @fn Value Value::operator-(const Value& val) const;
 * Overload of the subtraction operator.
 */


/**
 * Test if the current value is a subset of the passed.
 * @param x		Passed value.
 * @return		True if the current value is a subset of x, false else.
 */
bool Value::subsetOf(const Value& x) const {
	if(x.isTop() || x == *this)
		return true;
	uintn_t xb = x.start(), xd = elm::abs(x.delta());
	if(xd == 0)
		return false;
	uintn_t b = start(), d = elm::abs(delta());
	return (b - xb) % xd == 0
		&& d % xd == 0
		&& (b - xb) % xd <= x.mtimes();
}

/**
 * @fn intn_t Value::start(void) const;
 * @return the "start" of the CLP, i.e. the lower bound if delta >= 0,
 * lower + delta * mtimes else.
 */

/**
 * @fn intn_t Value::stop(void) const
 * @return the "stop" of the CLP, i.e. the upper bound if delta >= 0,
 *	lower else.
 */

/**
 * Multiply another set to the current one with the higher 32 bits
 * @param val the value to multiply
 */
Value& Value::mulh(const Value& val){
	if (_kind == NONE && val._kind == NONE) 	/* NONE + NONE = NONE */
		*this = none;
	else if (_kind != VAL || val._kind != VAL) 	/* ALL + anything = ALL */
		*this = all;
	else if (_delta == 0 && val._delta == 0) {	/* two constants */
		t::uint64 result = _base;
		result = result * val._base;
		result = result >> 32;
		_base = result;
	}
	else
		*this = all;

	return *this;
}


/**
 * Multiply another set to the current one with the lower 32 bits
 * @param val the value to multiply
 */
Value& Value::mul(const Value& val){
	if (_kind == NONE && val._kind == NONE) 	/* NONE + NONE = NONE */
		*this = none;
	else if (_kind != VAL || val._kind != VAL) 	/* ALL + anything = ALL */
		*this = all;
	else if (_delta == 0 && val._delta == 0) {	/* two constants */
		t::uint64 result = _base;
		result = result * val._base;
		result = result & 0xFFFF;
		_base = (intn_t)result;
	}
	else if((_delta == 0 || val._delta == 0) && (_mtimes != UMAXn) && (val._mtimes != UMAXn)) {
		Value k, nk;
		if(_delta == 0 || _mtimes == 0) {
			k = *this;
			nk = val;
		}
		else {
			k = val;
			nk = *this;
		}
		t::uint64 result = _base;
		result = result * val._base;
		result = result & 0xFFFF;
		_base = (intn_t)result;

		t::uint64 result_delta = nk._delta;
		result_delta = result_delta * k._base;
		result_delta = result_delta & 0xFFFF;
		_delta = (intn_t)result_delta;

		_mtimes = nk._mtimes;
	}
	else
		*this = all;

	return *this;
}

/**
 * Divide the current CLP with the given one and put the result in the current CLP.
 * @param val	Divisor.
 */
Value& Value::div(const Value& val){
	if (_kind == NONE && val._kind == NONE) 	/* NONE + NONE = NONE */
		return *this = none;
	else if (_kind != VAL || val._kind != VAL) 	/* ALL + anything = ALL */
		return *this = all;
	else if(isConst() && val.isConst())
		return *this = Value(VAL, _base / val._base);
	else if(isConst() || val.isConst()) {
		Value k, nk;
		if(_delta == 0 || _mtimes == 0) {
			k = *this;
			nk = val;
		}
		else {
			k = val;
			nk = *this;
		}
		if(nk._mtimes < ARITHMETIC_THRESHOLD) {
			Value result = Value::bot;
			for(unsigned int mt = 0; mt <= nk._mtimes; mt++) {
				int temp = (nk._base + nk._delta * mt) / k._base;
				result.join(Value(temp));
			}
			return *this = result;
		}
	}

	return *this = all;
}

/**
 * Compute the modulo the current CLP with the given one and put the result
 * in the current CLP.
 * @param val	Divisor.
 */
Value& Value::mod(const Value& val){
	if (_kind == NONE && val._kind == NONE)
		return *this = none;
	else if (val._kind != VAL || _kind == CMP)
		return *this = all;
	else if (val.isConst()) {
		if(_mtimes < ARITHMETIC_THRESHOLD) {
			Value result = Value::bot;
			for(unsigned int mt = 0; mt <= _mtimes; mt++) {
				int temp = (_base + _delta * mt) % val._base;
				result.join(Value(temp));
			}
			return *this = result;
		}
		else if(!isInf() && (upper() < val.lower()))
			return *this;
		else {
			return *this = Value(VAL, 0, 1, val.lower() - 1);
		}
	}
	else {
		return *this = Value(VAL, 0, 1, val.upper() - 1);
	}

	return *this;

}


/**
 * Add another set to the current one (result in the current CLP).
 * @param val the value to add
 */
Value& Value::add(const Value& val){
	if (_kind == NONE && val._kind == NONE) 	/* NONE + NONE = NONE */
		*this = none;
	else if (_kind != VAL || val._kind != VAL) 	/* ALL + anything = ALL */
		*this = all;
	else if (_delta == 0 && val._delta == 0) 	/* two constants */
		set(_kind, _base + val._base, 0, 0);
	else if(direction() == val.direction()) {
		if(isInf() || val.isInf()) { // same direction, either one of the component is inf
			intn_t g = gcd(_delta, val._delta);
			intn_t l = _base + val._base;
			set(VAL, l, g, UMAXn);
		}
		else { // same direction, other cases
			intn_t g = gcd(_delta, val._delta);
			intn_t l = _base + val._base;
			uintn_t m1 = _mtimes * (elm::abs(_delta) / elm::abs(g));
			uintn_t m2 = val._mtimes * (elm::abs(val._delta) / elm::abs(g));
			uintn_t mtimes =  m1 + m2;
			if(isInf() || val.isInf()) // if one of the value is with inf mtimes, then the result should be infinite too.
				mtimes = UMAXn;
			else if(UMAXn - m1 < m2) // check if m1 + m2 > UMAXn, however UMAXn is the largest value, so we use UMAXn - m1 to see if there is an overflow
				mtimes = UMAXn;
			set(VAL, l, g, mtimes);

		}
	}
	else {
		if(isInf() && val.isInf()) // diff direction, both of the component are inf
			*this = Value::all;
		else { // different direction
				Value temp(val);
				if(isInf()) 			// inf will take more effect
					temp.reverse(); 	// temp2 needs to follow temp1's direction
				else if (val.isInf())
					reverse();
				else if(_delta < 0) 	// if none of them is infinite, then make the negative delta to the positive one by reversing it
					reverse();
				else 					// same for the 2nd operand
					temp.reverse();
				intn_t g = gcd(_delta, temp._delta);
				intn_t l = _base + temp._base;
				uintn_t m1 = _mtimes * (elm::abs(_delta) / elm::abs(g));
				uintn_t m2 = temp._mtimes * (elm::abs(temp._delta) / elm::abs(g));
				uintn_t mtimes =  m1 + m2;
				if(isInf() || val.isInf()) // if one of the value is with inf mtimes, then the result should be infinite too.
					mtimes = UMAXn;
				else if((UMAXn - m1) < m2)
					mtimes = UMAXn;
				set(VAL, l, g, mtimes);
		}
	}
	return *this;
}

/**
 * Subtract another set to the current one
 * @param val the value to subtract
 */
void Value::sub(const Value& val) {
	if (_kind == NONE && val._kind == NONE)		/* NONE - NONE = NONE */
		*this = none;
	else if (_kind != VAL || val._kind != VAL)	/* ALL - anything = ALL */
		*this = all;
	else if (_delta == 0 && val._delta == 0)	/* two constants */
		set(_kind, _base - val._base, 0, 0);
	else {
		Value temp(val);
		temp._delta = -temp._delta;
		temp._base = -temp._base;
		add(temp);
	}
}


/**
 * Print a human representation of the CLP
 * @param out the stream to output the representation
 */
void Value::print(io::Output& out) const {
	if (_kind == ALL)
		out << 'T';
	else if (_kind == NONE)
		out << '_';
	else if(_kind == CMP)
		out << (_base < 0 ? 't' : 'r') << ::abs(_base)
			<< " ~ "
			<< (_delta < 0 ? 't' : 'r') << ::abs(_delta);
	else if ((_delta == 0) && (_mtimes ==  0))
		out << "k(0x" << io::hex(_base) << ')';
		//out << "k(" << _lower << ')';
	else {
		if(_base >= 0)
			out << "(0x" << io::hex(_base);
		else {
			intn_t _baseToPrint = 0 - _base;
			out << "(-0x" << io::hex(_baseToPrint);
		}
		if(_delta >= 0)
			out << ", 0x" << io::hex(_delta);
		else {
			intn_t _deltaToPrint =  0-_delta;
			out << ", -0x" << io::hex(_deltaToPrint);
		}
		if(_mtimes == uintn_t(-1))
			out << ", inf)";
		else
			out << ", 0x" << io::hex(_mtimes) << ')';
	}
}

/**
 * Logically left shift the current value.
 * @param val the value to shift the current one with. Must be a positive
 *				constant.
*/
void Value::shl(const Value& val) {
	if(_kind == NONE && val._kind == NONE)
		*this = bot;
	if(!val.isConst() || val._base < 0 || _kind != VAL || val._kind != VAL)
		set(top);
	else {
		if (_delta == 0 && _mtimes == 0) {
			// set(VAL, _base << val._base, 0, 0);
			long a = _base;
			long b = val._base;
			set(VAL, a << b, 0, 0);
		}
		else
			set(VAL, _base << val._base, _delta << val._base, _mtimes);
	}
}

/**
 * Logically right shift the current value.
 * @param val the value to shift the current one with. Must be a positive
 *				constant.
 */
Value& Value::shr(const Value& val) {
	if(!val.isConst() || val._base < 0 || _kind != VAL || val._kind != VAL){
		set(ALL, 0, 1, UMAXn);
	} else
	if (_kind != NONE && _kind != ALL) {
		if (_delta == 0 && _mtimes == 0) {
			//set(VAL, _base >> val._base, 0, 0);
			long a = _base;
			long b = val._base;
			set(VAL, a >> b, 0, 0);

		}
#		ifdef USE_ORIGINAL_SHR
		else if (_delta % 2 == 0)
			set(VAL, _base >> val._base, _delta >> val._base, _mtimes);
		else
			set(VAL, _base >> val._base, 1, (_delta * _mtimes) >> val._base);
#		else
		else if(_delta % (1 << val._base) == 0) {
			set(VAL, _base >> val._base, _delta >> val._base, _mtimes);
		}
		else {
			t::uint64 mtimes_new = _mtimes;
			mtimes_new = mtimes_new * elm::abs(_delta);
			mtimes_new = (mtimes_new >> val._base) + 1;
			intn_t delta_new;
			if(_delta >= 0)
				delta_new = 1;
			else
				delta_new = -1;
			
			// over-approximation
			if(_mtimes == UMAXn)
				mtimes_new = UMAXn;
			
			set(VAL, _base >> val._base, delta_new, mtimes_new);
		}
#		endif		
	}
	return *this;
}


/**
 * Aritmetically right shift the current value.
 * @param val	the value to shift the current one with. Must be a positive
 *				constant.
 */
Value& Value::asr(const Value& val) {
	if(_kind != ALL || val._kind != VAL || !val.isConst() || val._base < 0){
		set(ALL, 0, 1, UMAXn);
		return *this;
	}

	uintn_t mask = 1 << 31;
	uintn_t up = upper();
	uintn_t lo = lower();

	if((up & mask) || (lo & mask)) {
		// *this = Value(VAL, 0, 1, 0xffffffff);
		set(ALL, 0, 1, UMAXn);
		return *this;
	}
	else {
		shr(val);
		return *this;
	}
}


/**
 * Perform OR operator on values (modifying current one).
 * @param val	The value to OR with the current one.
 */
void Value::_or(const Value& val) {
	if(_kind != VAL)
		return;
	if(val.kind() != VAL) {
		*this = val;
		return;
	}
	if(val.isConst()) {
		if(isConst())
			_base |= val._base;
		/*else if(val._lower < _delta)
			_lower |= val._lower;*/		// TO CHECK
		else
			*this = all;
	}
	else {
		*this = all;
		/*if(OCLP_IS_CST(*this))
			set(V)
			&& _lower < val.delta())
			set(VAL, _lower | val._lower, val.delta(), val.mtimes());
		else
			*this = all;*/
	}
}


/**
 * @fn bool isTop(void) const;
 * Test if the value is top, that is, any value.
 * @return	True if it is top, false else.
 */


/**
 * Join another set to the current one
 * @param val the value to be joined with
 */
Value& Value::join(const Value& val) {
	if((*this) == val)							/* A U A = A (nothing to do) */
		return *this;
	else if (_kind == NONE)						/* NONE U A = A */
		set(VAL, val._base, val._delta, val._mtimes);
	else if (val._kind == NONE)					/* A U NONE = A (nothing to do) */
		return *this;
	else if (_kind != VAL || val._kind != VAL)  /* ALL U anything = ALL */
		set(ALL, 0, 1, UMAXn);
	else if(isConst() && val.isConst()) {/* k1 U k2 */
		if(val._base > _base)
			set(VAL, _base, val._base - _base, 1);
		else
			set(VAL, val._base, _base - val._base, 1);
	}
	else {										/* other cases */
		if(isConst() || val.isConst()) {
			// k is the constant value
			// v is the none constant value
			Value v, k;
			if(isConst()) {
				k = *this;
				v = val;
			}
			else {
				k = val;
				v = *this;
			}

			// now check if k is within v
			if(k.inter(v).kind() != NONE) {
				*this = v;
				return *this;
			}
		}

		if((isInf() && val.isInf())) {
			//  <--------------------------| *this join
			//                  |----------------------------------> val
			//  <--------------------------------------------------> = T (result)
			if((delta() >= 0 && val.delta() < 0) || (delta() < 0 && val.delta() >= 0)) {
				*this = ALL;
			}
			//             |-------------------------------> *this join
			//   |-----------------------------------------> val
			//   |-----------------------------------------> (result)
			// OR
			//   <------------------------| *this join
			//   <------------------------------| val
			//   <------------------------------| (result)
			else {
				intn_t new_delta = gcd(_delta, val._delta);
				intn_t new_base;
				if((new_delta > 0 && _base < val._base) || (new_delta < 0 && _base > val._base))
					new_base = _base;
				else
					new_base = val._base;
				set(VAL, new_base, new_delta, UMAXn);
			}
			return *this;
		}

		if(isInf() || val.isInf()) {
			Value a, b; // a is with inifite mtimes, b is not
			if(isInf()) {
				a = *this;
				b = val;
			}
			else {
				a = val;
				b = *this;
			}

			// make sure the delta will go with the infinite
			if((a.delta() >= 0 && b.delta() < 0) || (a.delta() < 0 && b.delta() >= 0)) {
				b.reverse();
			}

			intn_t new_delta = gcd(a.delta(), b.delta());
			//  |-------------------------------> a
			//       |---------------| b
			//  |-------------------------------> result
			// OR
			//          |-----------------------> a
			//  |----------| b
			//  |-------------------------------> result
			// OR
			//                   |--------------> a
			//  |------| b
			//  |-------------------------------> result
			if(a.delta() >= 0) {
				intn_t new_base;
				if(a.lower() < b.lower())
					new_base = a.lower();
				else
					new_base = b.lower();
				set(VAL, new_base, new_delta, UMAXn);
			}
			//  <-------------------------------| a
			//       |---------------| b
			//  <-------------------------------| result
			// OR
			//  <------------| a
			//            |----------| b
			//  <--------------------| result
			// OR
			//  <--------------| a
			//                           |------| b
			//  <-------------------------------- result
			else {
				intn_t new_base;
				if(a.lower() > b.lower())
					new_base = a.lower();
				else
					new_base = b.lower();
				set(VAL, new_base, new_delta, UMAXn);
			}
			return *this;
		}

		uintn_t g = gcd(gcd(elm::abs(start() - val.start()), _delta), val._delta);
		intn_t ls = min(start(), val.start());
		t::int64 u1 = t::int64(start()) + t::int64(elm::abs(_delta)) * t::int64(_mtimes);
		t::int64 u2 = t::int64(val.start()) +t::int64(elm::abs(val._delta)) * t::int64(val._mtimes);
		t::int64 umax;
		if (u1 > u2)
			umax = u1;
		else
			umax = u2;
		set(VAL, ls, g, (umax - ls) / g);
	}
	return *this;
}

/**
 * Perform a widening to the infinite (to be filtered later)
 * @param x the value of the next iteration state
*/
Value& Value::widening(const Value& x) {
	return ffwidening(x, -1);
}

/**
 * Perform a widening, knowing flow facts for the loop
 * @param x the value of the next iteration state
 * @param N the maximum number of iteration of the loop
*/
Value& Value::ffwidening(const Value& x, int N){
	
	// 	(0)	⊥ ▽ x = x ▽ ⊥ = x
	if(kind() == NONE)
		*this = x;
	else if(x.kind() == NONE)
		return *this;

	// (1)	⊤ ▽ x = x ▽ ⊤ = x
	else if(kind() != VAL || x.kind() != VAL)
		*this = all;

	// (2) x ▽ x = x
	else if (*this == x)
		return *this;

	// (2') (b, 0, 0) ▽ (b, δ, 1) = (b, δ, N)
	else if(N >= 0 && isConst() && x.mtimes() == 1)
		*this = Value(VAL, base(), x.delta(), N);

	// (2") (b, δ, N) ▽ (b, δ, N+1) = (b, δ, N)
	else if(N >= 0 && mtimes() == uintn_t(N) && x.mtimes() == uintn_t(N) + 1 && delta() == x.delta())
		return *this;

	// (3) (b, δ, n) ▽ (b', δ', n') =	(b, δ", ∞)
	// with δ" = gcd(δ, δ', b' - b) ∧ δ ≥ 0 ∧ δ' ≥ 0 ∧ b' - b ≥ 0
	else if(delta() >= 0 && x.delta() >= 0 && (x.base() - base()) >= 0) {
		auto delta_s = ugcd(ugcd(delta(), x.delta()), x.base() - base());
		*this = Value(VAL, base(), delta_s, UMAXn);
	}

	// (4) (b, δ, n) ▽ (b', δ', n') = (b, δ", ∞)
	// with δ" = -gcd(-δ, -δ', b - b') ∧ δ ≤ 0 ∧ δ' ≤ 0 ∧ b' - b ≤ 0
	else if(delta() <= 0 && x.delta() <= 0 && (x.base() - base()) <= 0) {
		auto delta_s = -ugcd(-ugcd(delta(), -x.delta()), base() - x.base());
		*this = Value(VAL, base(), delta_s, UMAXn);
	}

	// (5) (b, δ, n) ▽ (b', δ', n') = ⊤
	else
		*this = top;

	// normalize the representation
	if(kind() == VAL && (delta() == 0 || mtimes() == 0))
		set(VAL, base(), 0, 0);
	return *this;
}


/**
 * Intersection with the current value.
 * @param val the value to do the intersection with
 */
Value& Value::inter(const Value& val) {

	// In this function, numbers are refs to doc/inter/clpv2-inter.pdf

	// 2. Special cases ========================

	// 2.0 bottom
	if(((*this) == Value::none) || (val == Value::none)) {
		*this = Value::none;
		return *this;
	}

	// 2.1. A n A (T n T)
	if ((*this) == val)
		return *this;
	else if(isTop()) {
		*this = val;
		return *this;
	}
	else if(val.isTop())
		return *this;
	else if(_kind != VAL || val._kind != VAL) {
		*this = top;
		return *this;
	}

	// 2.2. cst n cst
	if(isConst() && val.isConst()){
		if(_base != val._base)
			set(NONE, 0, 0, 0);
		return *this;
	}

	// 2.3. cst n clp || clp n cst
	// TODO		direction problem?
	if (isConst()) {
		Value temp(val);
		if(!temp.direction())
			temp.reverse();

		long diff;
		if((temp.upper() > temp.lower()) || (_base > temp._base)) // normal case
			diff = (long)_base - (long)temp._base;
		else
			diff = (uintn_t)(_base) - (long)temp._base;

		// if the difference of the values is not a multiple of the delta, then the value does not fall on the interval
		if((diff % temp._delta) != 0) {
			*this = Value::none;
			return *this;
		}
		if((uintn_t)(diff / temp._delta) > temp._mtimes)
			*this = Value::none;
		else
			set(VAL, _base, 0, 0);
		return *this;
	}

	if(val.isConst()) {
		Value temp(*this);
		if(!temp.direction())
			temp.reverse();

		long diff;
		if((temp.upper() > temp.lower()) || (_base > temp._base)) // normal case
			diff = (long)val._base - (long)temp._base;
		else
			diff = (uintn_t)(val._base) - (long)temp._base;

		// if the difference of the values is not a multiple of the delta, then the value does not fall on the interval
		if((diff % temp._delta) != 0) {
			*this = Value::none;
			return *this;
		}

		if((uintn_t)(diff / temp._delta) > temp._mtimes) {
			*this = Value::none;
			return *this;
		}
		else
			set(VAL, val._base, 0, 0);
		return *this;
	 }


	// need to make sure they go to the same, and positive direction
	Value val1(*this);
	Value val2(val);
	if(!val1.direction())
		val1.reverse();
	if(!val2.direction())
		val2.reverse();

	// 2.5 now carry out intersection, first we take care of the circularity and overflow as section 6 in [Sen et Srikant, 2007]
	// if upperbound < lowerbound, means circularity and overflow
	intn_t val1upper = val1._base + val1._delta * val1._mtimes;
	intn_t val2upper = val2._base + val2._delta * val2._mtimes;
	if((val1upper < val1._base) || (val2upper < val2._base)) {
		static int iii = 0;
		iii++;
		ASSERT(iii == 1);
		// make P and Q for A and B
		Value PA, PB, QA, QB, PA2, QA2;
		val1.PQValue(PA, QA);
		val2.PQValue(PB, QB);
		PA2 = PA;
		QA2 = QA;

		// intersection for each component
		PA.inter(PB);
		PA2.inter(QB);
		QA.inter(PB);
		QA2.inter(QB);

		// join the results
		PA.join(PA2).join(QA).join(QA2);
		*this = PA;
		iii--;
		return *this;
	}

	// 2.5.0 not overlapping intervals
	// the one with lower start, should have enough mtimes to catch up the start of the other
	if(	((val1._base > val2._base) && ((elm::abs(val1._base - val2._base) / val2._delta) > val2._mtimes)) ||
		((val2._base > val1._base) && ((elm::abs(val2._base - val1._base) / val1._delta) > val1._mtimes)) ) {
		set(NONE, 0, 0, 0);
		return *this;
	}

	// 2.5.1 then for non overflowing CLP values, we take similar steps in 5.1 of [Sen et Srikant, 2007]
	intn_t u1x, u2x; // upper bound of the CLP values
	intn_t resultBase; // the lower bound of the resulting CLP
	uintn_t resultMtimes, resultDelta; // the mtimes and delta for the result
	// for following the assumption, largerBaseV._base > smallerBaseV.base
	Value largerBaseV, smallerBaseV;
	if(val1._base >= val2._base) {
		largerBaseV = val1;
		smallerBaseV = val2;
	}
	else {
		largerBaseV = val2;
		smallerBaseV = val1;
	}
	// find the new delta
	resultDelta = lcm(largerBaseV._delta, smallerBaseV._delta);
	// find the minimal j'
	uintn_t j = 0;
	bool foundj = false;
	for(j = 0; j < (uintn_t)smallerBaseV._delta; j++) {
		if((largerBaseV._base - smallerBaseV._base + j * largerBaseV._delta) % smallerBaseV._delta == 0) {
			foundj = true;
			break;
		}
	}

	if(foundj == false) {
		*this = none;
		return *this;
	}

	// find the new base
	resultBase = largerBaseV._base + j * largerBaseV._delta;
	// check if the new base is lower than the upper bounds of the both CLP
	u1x = smallerBaseV._base + smallerBaseV._delta * smallerBaseV._mtimes;
	u2x = largerBaseV._base + largerBaseV._delta * largerBaseV._mtimes;
	if(resultBase > min(u1x,u2x)) {
		set(NONE, 0, 0, 0);
		return *this;
	}
	// find the new mtimes
	resultMtimes = (min(u1x, u2x) - resultBase) / resultDelta;
	if(resultMtimes == 0) // in case mtimes is 0, which means a constant value
		resultDelta = 0;

	val1.set(VAL, resultBase, resultDelta, resultMtimes);

	// if both directions are the same, we keep the direction
	if(direction() == val.direction())
		*this = val1;
	// if different directions, we need to have positive delta
	else if (!val1.direction()) {
		val1.reverse();
		*this = val1;
	}
	else
		*this = val1;
	return *this;
}

/**
 * Reverse the CLP direction (swap upper and lower bounds, and use
 * the opposite of delta as new delta).
*/
void Value::reverse(void){
	if(_kind == VAL)
		set(clp::VAL, _base + _delta * _mtimes, -_delta, _mtimes);
}


/**
 * Filter the current value with signed values greater than k.
 * @param k		Pivot value.
 */
void Value::greater_or_equal(intn_t k) {

	// all cases
	if(*this == all) {
		*this = Value(VAL, k, 1, MAXn-k);
		return;
	}

	// except value
	if(_kind != VAL) {
		*this = top;
		return;
	}

	// case of constant
	if(isConst()) {
		if(k > _base)
			*this = none;
		return;
	}

	// d >= 0 => inter((b, d, n), (k, 1, inf+ - k)
	if(_delta > 0) {
		inter(Value(VAL, k, 1, MAXn - k));
		return;
	}

	// d < 0 !!!
	// if wrapping, change the current value for no wrapping
	if(swrap())
		_mtimes = (MAXn - k) / (-_delta);

	// b <= k -> _
	if(_base <= k) {
		*this = none;
		return;
	}

	// b + dn >= k -> (b, d, n)
	if(_base + _delta * intn_t(_mtimes) >= k)
		return;

	// _ -> (b, d, (k - b) / d
	else
		_mtimes = (k - _base) / _delta;

	check();
	return;
}


/**
 * Filter the current value with signed values greater or equal than x.
 * @param x		Compared value.
 */
Value& Value::ge(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.swrap())
		return *this;
	
	// compute
	greater_or_equal(x.start());
	return *this;
}


/**
 * Filter the current value with signed values stricly greater than x.
 * @param x		Compared value.
 */
Value& Value::gt(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.swrap())
		return *this;
	
	// compute
	greater_or_equal(x.start() + 1);
	return *this;
}


/*
 * LE logic
 * ========
 *
 * ASSUMPTION: used algorithms does not wrap!
 * Without such an assumption, filter becomes inefficient.
 * LEMMA: restrict (l, d, n) to non-wrapping part.
 * 		if d >= 0,	(l, d, (+inf - l) / d)
 * 		else		(l, d, (-inf - l) / d)
 *
 * 3 cases
 * 		)####----(		{ x <= k }
 * 		)-----##-(		case a
 * 		)--####--(		case b
 * 		)-##-----(		case c
 * 	case a: _ 								(no intersection)
 * 	case c: (b, l, n)						(identity)
 * 	case b: (l, d, (k - l)/d) 	if d >= 0	(forward intersection)
 *			(l + ds, d, d - ks	if d < 0	(backward intersection)
 *			with s = (l - k + d + 1) / d
 */

/**
 * Filter the current value with signed values lesser than k.
 * @param k		Threshold.
 */
void Value::less_or_equal(intn_t k) {
	
	// non-value cases
	if(_kind == NONE)
		return;
	
	// make the value from ALL to [k, -inf]
	else if(_kind != VAL) {
		// first obtain the difference between k and -inf (MINn), use a 64 bit value to prevent overflow
		t::int64 m = k;
		m = m - MINn;
		Value temp(VAL, k, -1, m);
		// since we have ALL, there is no way to know the direction, so we make it positive d = 1
		temp.reverse();
		*this = temp;
		return;
	}

	// const case
	if(isConst()) {
		if(k < _base)
			*this = none;
		return;
	}

	// simple cases
	if(*this == all || *this == none)
		return;
	if(isConst()) {
		if(k < _base)
			*this = none;
		return;
	}

	// not so simple
	else {

		// wrap fix
		if(swrap()) {
			if(_delta >= 0)
				_mtimes = (uintn_t(MAXn) - _base) / _delta;
			else
				_mtimes = (uintn_t(MINn) + _base) / (-_delta);
		}

		// apply le
		if(start() > k)		// case a: if the min-possible value is larger than filter, than nothing will left
			*this = none;
		else if(stop() < k)	// case c: if the max-possible value if smaller than the filter, than all can pass
			return;
		else {				// case b: start <= k && stop >= k
			if(_delta >= 0) {
				_mtimes = (k - _base) / _delta;
			}
			else {
				intn_t s = (_base - k - _delta - 1) / (-_delta);
				_base = _base + _delta * s; // need to make sure the _base is smaller than the filter too
				_mtimes -= s;
			}
			ASSERTP(start() <= k,  *this << " should be ≤ " << k << ", but start() = " << start() << " and stop() = " << stop() << io::endl);
			ASSERTP(stop() <= k,  *this << " should be ≤ " << k << ", but start() = " << start() << " and stop() = " << stop() << io::endl);
		}
	}

	check();
}



/**
 * Filter the current value with signed values lesser or equal than x.
 * @param x		Value to filter with.
 */
Value& Value::le(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.swrap())
		return *this;
	
	// get the pivot value
	less_or_equal(x.stop());
	return *this;
}


/**
 * Filter the current value with signed values strictly lesser than x.
 * @param x		Value to filter with.
 */
Value& Value::lt(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.swrap())
		return *this;
	
	// get the pivot value
	less_or_equal(x.stop() - 1);
	return *this;
}


/**
 * Filter the current value with unsigned values greater than k.
 * @param k		Threshold.
 */
void Value::unsigned_greater_or_equal(uintn_t k) {

	// non-value cases
	if(_kind == NONE)
		return;
	else if(_kind != VAL) {
		*this = Value(VAL, k, 1, UMAXn-k);
		return;
	}

	// none cases
	if(*this == none)
		return;

	// case of constant
	if(isConst()) {
		if(k > uintn_t(_base))
			*this = none;
		return;
	}

	if(_delta > 0) {
		// see if we need to replace the _base
		intn_t new_base = _base;
		if(((uintn_t)_base) < k)
			new_base = k;
		// calculate the upper bound
		t::uint64 v = _delta;
		v = _delta * _mtimes;
		v = v + _base;
		if(v < k) { // means the whole CLP is less than k
			*this = none;
			return;
		}
		// find the new mtimes
		uintn_t new_mtimes = (v - new_base) / _delta;
		*this = Value(VAL, new_base, _delta, new_mtimes);
		return;
	}
	else {
		reverse();
		// see if we need to replace the _base
		intn_t new_base = _base;
		if(((uintn_t)_base) < k)
			new_base = k;
		// calculate the upper bound
		t::uint64 v = _delta;
		v = _delta * _mtimes;
		v = v + _base;
		if(v < k) { // means the whole CLP is less than k
			*this = none;
			return;
		}
		// find the new mtimes
		uintn_t new_mtimes = (v - new_base) / _delta;
		*this = Value(VAL, new_base, _delta, new_mtimes);
		reverse();
		return;
	}
}


/**
 * Filter the current value with unsigned values greater or equal than x.
 * @param x		Value to filter with.
 */
Value& Value::geu(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.uwrap())
		return *this;
	
	// get the pivot value
	unsigned_greater_or_equal(x.ustart());
	return *this;
}


/**
 * Filter the current value with unsigned values greater or equal than x.
 * @param x		Value to filter with.
 */
Value& Value::gtu(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.uwrap())
		return *this;
	
	// get the pivot value
	unsigned_greater_or_equal(x.ustart() + 1);
	return *this;
}


/**
 * Filter the current value with unsigned values lesser than k.
 * @param k		Threshold.
 */
void Value::unsigned_less_or_equal(uintn_t k) {

	// nothing to filter will be nothing still
	if(*this == none)
		return;

	// for un-signed case, ALL will be filtered to [0, k]
	if(_kind != VAL) {
		*this = Value(VAL, 0, 1, k);
		return;
	}

	// case of constant
	if(isConst()) {
		if(k < uintn_t(_base))
			*this = none;
		return;
	}

	// d < 0 => inter((b, d, n), (k, 1, inf+ - k)
	if(_delta < 0) {
		inter(Value(VAL, 0, 1, k));
		return;
	}

	// d > 0 !!!
	// if wrapping, change the current value for no wrapping
	if(uwrap())
		_mtimes = k / _delta;


	// check if 0 falls in the middle of the range (zero-crossing)
	// then move the base to be >= 0
	if(_base < 0 && (_base + _delta * _mtimes) > 0) {
		intn_t newbase = (((0 - _base) / _delta)) * _delta + _base;
		if(newbase < 0)
			newbase = newbase + _delta;
		_base = newbase;
		// calculate the new mtimes
		_mtimes = (k - uintn_t(_base)) / _delta;
		return;
	}

	// b >= k -> _
	if(uintn_t(_base) >= k) {
		*this = none;
		return;
	}

	// b + dn >= k -> (b, d, n)
	if(uintn_t(_base + _delta * _mtimes) <= k)
		return;

	// _ -> (b, d, (k - b) / d
	else
		_mtimes = (k - uintn_t(_base)) / _delta;

	check();
}


/**
 * Filter the current value with unsigned values less or equal than x.
 * @param x		Value fo filter with.
 */
Value& Value::leu(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.uwrap())
		return *this;
	
	// filter
	unsigned_less_or_equal(x.ustop());
	return *this;
}


/**
 * Filter the current value with unsigned values stricly less than x.
 * @param x		Value fo filter with.
 */
Value& Value::ltu(const Value& x) {

	// non case for x
	if(x.isBot()) {
		*this = bot;
		return *this;
	}
	
	// non-value or signed wrapping
	if(!x.isValue() || x.uwrap())
		return *this;
	
	// filter
	unsigned_less_or_equal(x.ustop() - 1);
	return *this;
}


/**
 * Filter the value to b equal to x.
 */
Value& Value::eq(const Value& x) {
	if(isBot())
		;
	else if(x.isBot()) {
		*this = bot;
		return *this;
	}
	else if(x.isConst() && (isTop() || x.subsetOf(*this)))
		*this = x;
	else
		*this = top;
	return *this;
}

/**
 * Filter the value to be not equal to k.
 */
Value& Value::ne(const Value& x) {
	if(isBot())
		;
	else if(x.isBot()) {
		*this = bot;
		return *this;
	}
	else if(x.isConst() && isConst() && base() == x.base())
		*this = none;
	else if(x.isComp() || isComp())
		*this = top;
	return *this;
}


/**
 * Threshold giving the maximum size of a CLP set
 * to apply AND explicitly on the whole set
 * (and rebuilding a new CLP value).
 */
int Value::and_threshold = 8;

/**
 * Perform AND on the current value.
 * @param val	Value to perform AND on.
 */
Value& Value::_and(const Value& val) {

	// if both of them are all, return all
	if(isTop() && val.isTop())
		return *this;

	// _ & v = v & _ = _
	if(isBot())
		return *this;
	if(val.isBot()) {
		*this = bot;
		return *this;
	}
	
	// condition case
	if(isComp() || val.isComp()) {
		*this = top;
		return *this;
	}

	// check for any constant
	Value v; // the CLP value
	uintn_t k; // the constant value to apply the AND relation on v
	if(isConst()) {
		if(val.isConst()) {		// k1 & k2
			*this = val.lower() & lower();
			return *this;
		}
		k = lower();
		v = val;
	}
	else if(val.isConst()) {
		v = *this;
		k = val.lower();
	}
	else {						// no k : cannot compute
		*this = all;
		return *this;
	}

	// v & 0 = 0
	if(k == 0) {
		*this = 0;
		return *this;
	}


	// for any value, m is the least significant bit of 1, and n is the most significant bit of 1
	// 000000000111110000000
	//          n   m
	// 000000000000000100110
	//                n   m
	// 111111111111100000000
	// n           m

	// first find the m and n for the constant k
	int n_k = 0, m_k = 0;
	int mode = 0;
	intn_t temp = k;
	for (int i = 0; i < 32; i++) {
		if ((mode == 0) && ((temp & 1) == 1)) {
			mode = 1;
			m_k = i;
			n_k = i;
		}
		else if ((mode == 1) && ((temp & 1) == 1)) {
			n_k = i;
		}
		temp = temp >> 1;
	}

	// find the maximum bit for the v.upper() unsigned
	uintn_t vu = v.upper();
	int n_vu = 0;
	temp = 1 << 31;
	for (int i = 31; i >=0 ; i--) {
		if(vu & temp) {
			n_vu = i;
			break;
		}
		temp = temp >> 1;
	}

	// find the maximum bit for the v.lower() unsigned
	uintn_t vl = v.lower();
	int n_vl = 0;
	temp = 1 << 31;
	for (int i = 31; i >=0 ; i--) {
		if(vl & temp) {
			n_vl = i;
			break;
		}
		temp = temp >> 1;
	}

	// n_v is the possible significant bit of the v
	int n_v = 0;
	if(n_vl > n_vu)
		n_v = n_vl;
	else
		n_v = n_vu;

	// narrow down the effective range to lower n of v and k
	int n = 0;
	if(n_v > n_k)
		n = n_k;
	else
		n = n_v;

#ifndef COMPLEX_AND

	if(m_k > n_v) // none of the bit can be masked
		*this = Value(0);
	else // The delta is the lowest possible 1, i.e. the m bit. Because any value below the m bit will be 0.
		*this = Value(VAL, 0, (1 << m_k), (1 << (n - m_k + 1))-1);

	return *this;

#else
	// T & v = v & T = T
	if(v == all) {
		STAT_UINT x = 1;
		x = (x << (n_k + 1 - m_k));
		x = x - 1;
		*this = Value(VAL, 0, 1 << m_k, x);
		return *this;
	}

	// first get the m of m_v_base and m_v_delta
	// m_v_base
	int m_v_base = -1;
	intn_t v_base = v._base;
	for(int i = 0; i < 32; i++) {
		if(v_base & 1) {
			m_v_base = i;
			break;
		}
		v_base = v_base >> 1;
	}


	int m_v_delta = -1;
	intn_t v_delta = v._delta;
	for(int i = 0; i < 32; i++) {
		if(v_delta & 1) {
			m_v_delta = i;
			break;
		}
		v_delta = v_delta >> 1;
	}


	int m_v;
	if(m_v_base == -1) // if base is 0, then we take the m of delta
		m_v = m_v_delta;
	else if(m_v_base < m_v_delta) // f base has smaller m value, we take it
		m_v = m_v_base;
	else
		m_v = m_v_delta;


	// then we look for max(m_v, m)
	int m;
	if(m_v > m_k)
		m = m_v;
	else
		m = m_k;


	STAT_UINT v_max_cand1 = v._base;
	STAT_UINT v_max_cand2 = v._base + v._delta * v.mtimes();
	STAT_UINT v_max;
	if(v_max_cand1 > v_max_cand2)
		v_max = v_max_cand1;
	else
		v_max = v_max_cand2;


	// find n_v
	int n_v = 0;
	STAT_UINT v_max_comp = (STAT_UINT)1 << 63;
	for(int i = 63; i >= 0; i--) {
		if(v_max_comp & v_max) {
			n_v = i;
			break;
		}
		v_max_comp = v_max_comp >> 1;
	}


	// find the mtimes
	// first find the max possible value for the result
	STAT_UINT max_possible_value = 1;
	max_possible_value = max_possible_value << (n_v + 1);
	max_possible_value = max_possible_value - 1;
	STAT_UINT max_possible_value2 = 1;
	max_possible_value2 = max_possible_value2 << (n_k + 1);
	max_possible_value2 = max_possible_value2 - 1;
	max_possible_value = max_possible_value & max_possible_value2;


	// obtain the mtimes
	max_possible_value = max_possible_value >> m;


	// if mtimes is 0, then we set delta to 0 too, and the resulted value is 0
	if(max_possible_value == 0)
		*this = Value(VAL, 0, 0, 0);
	else
		*this = Value(VAL, 0, (1 << m), max_possible_value);

	return *this;
#endif
}

/**
 * @fn Value::void set(kind_t kind, intn_t lower, intn_t delta, uintn_t mtimes)
 * Set the values for the current object
 * @param kind the kind of the object
 * @param lower the lower bound of the CLP
 * @param delta the step of the CLP
 * @param mtimes the number of times delta need to be added to get the
 *				 max bound of the CLP
 */

/**
 * Represents the $\bot$ value.
 */
const Value Value::none(NONE), Value::all(ALL, 0, 1, UMAXn);

/**
 * Represents the $\top$ value.
 */
const Value Value::bot(NONE), Value::top(ALL, 0, 1, UMAXn);

} }	// otawa::clp
