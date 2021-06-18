/*
 *	CLP Value class interface
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

#ifndef OTAWA_DATA_CLP_VALUE_H_
#define OTAWA_DATA_CLP_VALUE_H_

#include <elm/types.h>
#include <elm/type_info.h>
#include <elm/io.h>
#include <elm/assert.h>

namespace otawa { namespace clp {

using namespace elm;

typedef t::int32 intn_t;
typedef t::uint32 uintn_t;
const int NBITS = sizeof(intn_t) * 8;
typedef t::uint64 STAT_UINT;
const uintn_t UMAXn = elm::type_info<uintn_t>::max;
const intn_t MAXn = elm::type_info<intn_t>::max;
const intn_t MINn = elm::type_info<intn_t>::min;

typedef enum {
	NONE,
	REG,
	VAL,
	ALL
} kind_t;

class Value {
public:
	static int and_threshold;

	inline Value(kind_t kind=VAL, intn_t lower=0, intn_t delta=0, uintn_t mtimes=0)
		: _kind(kind), _base(lower), _delta(delta), _mtimes(mtimes) { check(); }	
	inline Value(const int val):
		_kind(VAL), _base(val), _delta(0), _mtimes(0) {}
	inline static Value R(int n) { return Value(REG, n); }
	inline static Value addr(uintn_t base, intn_t delta = 0, uintn_t mtimes = 0) { return Value(VAL, base, delta, mtimes); }

	// accessors
	inline kind_t kind(void) const { return _kind; }
	inline intn_t delta(void) const { return _delta; }
	inline intn_t base() const { return _base; }
	inline uintn_t mtimes(void) const { return _mtimes; }

	inline bool isTop() const { return _kind == ALL; }
	inline bool isBot() const { return _kind == NONE; }
	inline bool isConst(void) const { return delta() == 0 || mtimes() == 0; }
	inline bool isInf(void) const { return (_mtimes == UMAXn); }

	inline intn_t lower(void) const { return _base; }
	inline intn_t upper(void) const { return _base + _delta * _mtimes; }
	inline bool direction(void) const { return (delta() > 0); }

	inline intn_t start(void) const
		{ if (_delta < 0) return upper(); else return _base; }
	inline intn_t stop(void) const
		{ if (_delta < 0) return _base; else return upper(); }

	// semantic operations
	Value& add(const Value& val);
	void sub(const Value& val);
	void shl(const Value& val);
	Value& shr(const Value& val);
	Value& asr(const Value& val);
	Value& mul(const Value& val);
	Value& mulh(const Value& val);
	Value& div(const Value& val);
	Value& mod(const Value& val);
	void _or(const Value& val);
	Value& _and(const Value& val);

	// comparisons
	Value& ge(intn_t k);
	Value& geu(uintn_t k);
	Value& le(intn_t k);
	Value& leu(uintn_t k);
	void eq(uintn_t k);
	void ne(uintn_t k);

	// AI operations
	bool subsetOf(const Value& val) const;
	Value& join(const Value& val);
	Value& widening(const Value& val);
	Value& ffwidening(const Value& val, int loopBound);
	Value& inter(const Value& val);

	// operator overloading
	inline Value& operator=(const Value& val)
		{ set(val._kind, val._base, val._delta, val._mtimes); return *this; }
	inline bool operator==(const Value& val) const {
		return _kind == val._kind
			&& _base == val._base
			&& _delta == val._delta
			&& _mtimes == val._mtimes;
	}
	inline bool operator!=(const Value& val) const { return !operator==(val); }
	inline Value operator+(const Value& val) const { Value v = *this; v.add(val); return v; }
	inline Value operator-(const Value& val) const { Value v = *this; v.sub(val); return v; }

	// constants
	static const Value none;
	static const Value bot;
	static const Value all;
	static const Value top;

	// class operations
	inline void set(kind_t kind, intn_t lower, intn_t delta, uintn_t mtimes){
		_kind = kind;
		_base = lower;
		_delta = delta;
		_mtimes = mtimes;
		check();
	}
	void print(io::Output& out) const;

	inline bool swrap(void) const
		{ return _delta != 0 && _mtimes > (MAXn - _base) / elm::abs(_delta); }
	inline bool uwrap(void) const
		{ return _delta != 0 && _mtimes > (UMAXn - _base) / elm::abs(_delta); }

	// ugly
	inline bool operator>=(const int val) const { return _base >= val; }

private:
	inline void check(void) { /*ASSERT((_delta == 0 && _mtimes == 0) || (_delta != 0 && _mtimes != 0));*/ }
	void PQValue(Value &p, Value &q);
	void reverse(void);

	kind_t _kind;
	intn_t _base;
	intn_t _delta;
	uintn_t _mtimes;
};

inline elm::io::Output& operator<<(io::Output& out, const Value &val)
	{ val.print(out); return out; }

} }		// otawa::clp

#endif /* OTAWA_DATA_CLP_VALUE_H_ */
