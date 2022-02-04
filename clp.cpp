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
 * \f[
 *	(b, \delta, n) = \{ b + i \times \delta ~|~ 0 \le i \le n \}
 * \f]
 *
 * This kind of value is specially useful to represent simple addresses and
 * array addresses (under the constraint of alignment).
 * 
 * 
 * @par Analyses
 * 
 * The main analysis is invoked by the feature otawa::clp::ANALYSIS_FEATURE.
 * Its result are available with the interface otawa::clp::Manager:
 * ```
 * auto manager = otawa::clp::ANALYSIS_FEATURE.get(workspace);
 * ```
 * 
 * CLP analysis may optionaly be benefit from other data flow analyses
 * (that must be invoked before to be effective):
 * * otawa::pred::FILTER_FEATURE -- improve condition filtering.
 * * otawa::ipet::FLOW_FACTS_FEATURE -- uses of loop bounds and accesses ranges.
 * 
 * 
 * @par Configuration
 * 
 * The following configuration items can help improve precision:
 * * otawa::ACCESS_RANGE -- limit the range of memory accesses (specially
 *   useful if the CLP analysis cannot estimate them precisely).
 * * otawa::dfa::MEM_INIT -- initial value for memory cells.
 * * otawa::dfa::REG_INIT -- initial value for registers.
 * 
 * @par References
 * * Sen, R., Srikant, Y. N. _WCET estimation for executables in the presence of
 *   data caches_. EMSOFT'2007.
 * * Cassé, H., Birée, F., Sainrat, P. _Multi-architecture value analysis for 
 *   machine code_. WCET 2013.
 */

/// plug-in descriptor
class Plugin: public ProcessorPlugin {
public:
	Plugin(void): ProcessorPlugin("otawa::clp", Version(2, 0, 0), OTAWA_PROC_VERSION) { }
};

} } // otawa::icat

otawa::clp::Plugin otawa_clp;
ELM_PLUGIN(otawa_clp, OTAWA_PROC_HOOK);
