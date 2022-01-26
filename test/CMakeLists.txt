execute_process(COMMAND "${OTAWA_CONFIG}" --make-app --libs -r otawa/clp
	OUTPUT_VARIABLE TEST_OTAWA_LDFLAGS 	OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "TEST_OTAWA_LDFLAGS=${TEST_OTAWA_LDFLAGS}")
	
add_executable(test_value "test_value.cpp")
set_property			(TARGET "test_value" PROPERTY COMPILE_FLAGS "${OTAWA_CFLAGS}")
target_link_libraries	("test_value" "${TEST_OTAWA_LDFLAGS}")
