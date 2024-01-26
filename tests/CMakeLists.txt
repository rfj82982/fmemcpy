file(GLOB files_test test_fmemcpy.f90)
include_directories(${CMAKE_SOURCE_DIR}/src)

add_executable(test_fmemcpy ${files_test})
target_include_directories(test_fmemcpy PRIVATE ${PROJECT_BINARY_DIR}/src)
target_link_libraries(test_fmemcpy PRIVATE fmemcpy)

file(GLOB files_evaluate evaluate_fmemcpy.f90)
include_directories(${CMAKE_SOURCE_DIR}/src)

add_executable(evaluate_fmemcpy ${files_evaluate})
target_include_directories(evaluate_fmemcpy PRIVATE ${PROJECT_BINARY_DIR}/src)
target_link_libraries(evaluate_fmemcpy PRIVATE fmemcpy)

## Run the test(s)
#set(run_dir "${test_dir}/init_test")
#message(STATUS "Example dir ${run_dir}")
#file(MAKE_DIRECTORY ${run_dir})
#if (BUILD_TARGET MATCHES "gpu")
#  file(COPY bind.sh DESTINATION ${run_dir})
#  add_test(NAME init_test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ./bind.sh $<TARGET_FILE:init_test> ${TEST_ARGUMENTS} WORKING_DIRECTORY ${run_dir})
#else ()
#  add_test(NAME init_test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:init_test> ${TEST_ARGUMENTS} WORKING_DIRECTORY ${run_dir})
#endif()
