/*
 *	clp plugin hook
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2020, IRIT UPS.
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

#include <otawa/proc/ProcessorPlugin.h>

using namespace elm;
using namespace otawa;

namespace otawa { namespace clp {

/**
 * @mainpage	OTAWA CLP plug-in
 * This plug-in value analysis according to Circular Linear Progressive (CLP)
 * approach. Values are expressed as triples $(b, \delta, n)$ representing
 * values of the set:
 * 
 * 		$(b, \delta, n) = { b + i \times \delta ~|~ 0 \le i \le n }$
 *
 * This kind of value is specially useful to represent simple addresses and
 * array addresses (under the constraint of alignment).
 * 
 * @par References
 */

/// plug-in descriptor
class Plugin: public ProcessorPlugin {
public:
	Plugin(void): ProcessorPlugin("otawa::clp", Version(1, 0, 0), OTAWA_PROC_VERSION) { }
};

} } // otawa::icat

otawa::clp::Plugin otawa_clp;
ELM_PLUGIN(otawa_clp, OTAWA_PROC_HOOK);
