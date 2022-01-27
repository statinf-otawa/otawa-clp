CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
project("otawa-clp")

# configuration
set(SOURCES
		"clp.cpp"
		"clp_Value.cpp"
		"clp_State.cpp"
		"clp_Analysis.cpp"
		"clp_Domain.cpp"
		"sem_predicates.cpp"
#		"sem_BackwardPredicateBuilder.cpp"
		"sem_ForwardPredicateBuilder.cpp"
		"OwnershipPtr.h"
		)

# C++ flags
if(CMAKE_BUILD_TYPE MATCHES Release)
	add_definitions(-DNDEBUG)
endif()
add_compile_options(-Wall)

# look for OTAWA
if(NOT OTAWA_CONFIG)
	find_program(OTAWA_CONFIG "otawa-config" DOC "path to otawa-config")
endif()
execute_process(COMMAND "${OTAWA_CONFIG}" --cmake OUTPUT_VARIABLE OTAWA_CMAKE)
string(STRIP "${OTAWA_CMAKE}" OTAWA_CMAKE)
include("${OTAWA_CMAKE}")

# build all
OTAWA_PLUGIN("clp" "otawa" "${SOURCES}")
OTAWA_INSTALL_INCLUDE("otawa")

# tests
if(WITH_TEST)
	add_subdirectory(test)
endif()

