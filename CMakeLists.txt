cmake_minimum_required(VERSION 3.20)                        
cmake_policy(SET CMP0074 NEW)

project(fmemcpy LANGUAGES Fortran C)

set(AUTHOR "Stefano RolfoPaul Bartholomew")
set(AUTHOR_DETAILS "stefano.rolfo@stfc.ac.uk;p.bartholomew@epcc.ed.ac.uk")
set(DESCRIPTION "Building fmemcpy using cmake")

message(STATUS "building ${PROJECT_NAME}")

option(ENABLE_OPENMP "Allow user to activate/deactivate openMP support" ON)

include(GNUInstallDirs)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "${PROJECT_BINARY_DIR}/opt" CACHE PATH "..." FORCE)
endif()

# Add support for CMAKE_DEPENDENT_OPTION
INCLUDE(CMakeDependentOption)
INCLUDE(CMakeParseArguments)

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Dev Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake" "${CMAKE_SOURCE_DIR}/cmake/compilers")

include(FMC_Compilers)

# Add the sources
add_subdirectory(src)

# Add the test 
add_subdirectory(tests)


