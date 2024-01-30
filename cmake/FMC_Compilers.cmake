# Compilers CMakeLists

set(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER_ID} )
set(C_COMPILER_NAME ${CMAKE_C_COMPILER_ID} )
message(STATUS "Fortran compiler name ${Fortran_COMPILER_NAME}")
message(STATUS "Fortran compiler version ${CMAKE_Fortran_COMPILER_VERSION}")
message(STATUS "C compiler name ${C_COMPILER_NAME}")
message(STATUS "C compiler version ${CMAKE_C_COMPILER_VERSION}")

if (ENABLE_OPENMP)
  find_package(OpenMP)
  if(OpenMP_Fortran_FOUND)
    message(STATUS "OpenMP for Fotran Compiler Found, version ${OpenMP_Fortran_VERSION_MAJOR}.${OpenMP_Fortran_VERSION_MINOR}")
  else()
    message(ERROR_CRITICAL "No OpenMP support detected")
  endif()

endif()

if (Fortran_COMPILER_NAME MATCHES "GNU")
  # gfortran
  message(STATUS "Setting gnu flags")
  include(FMC_flags_gnu)
elseif (Fortran_COMPILER_NAME MATCHES "Intel")
  message(STATUS "Setting intel flags")
  include(FMC_flags_intel)
elseif (Fortran_COMPILER_NAME MATCHES "Cray")
  message(STATUS "Setting cray  flags")
  include(FMC_flags_cray)
elseif (Fortran_COMPILER_NAME MATCHES "NVHPC")
	message(STATUS "Setting NVHPC SDK flags")
  include(FMC_flags_nvidia)
elseif (Fortran_COMPILER_NAME MATCHES "Fujitsu")
  message(STATUS "Setting Fujitsu flags")
  include(FMC_flags_fujitsu)
else (Fortran_COMPILER_NAME MATCHES "GNU")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("C compiler: " ${C_COMPILER_NAME})
  message ("No optimized Fortran/C compiler flags are known, we just try -O2...")
  set(FMC_FFLAGS_RELEASE "-O2 -g")
  set(FMC_FFLAGS_DEBUG   "-O0 ")
  set(FMC_CFLAGS_RELEASE "-O2 -g ")
  set(FMC_CFLAGS_DEBUG   "-O0 ")
endif (Fortran_COMPILER_NAME MATCHES "GNU")

if (NOT FLAGS_SET)
  set(CMAKE_Fortran_FLAGS ${FMC_FFLAGS} CACHE STRING 
	"Base FFLAGS for build" FORCE)
  set(CMAKE_Fortran_FLAGS_RELEASE ${FMC_FFLAGS_RELEASE} CACHE STRING
  	"Additional FFLAGS for Release (optimised) build" FORCE)
  set(CMAKE_Fortran_FLAGS_DEBUG ${FMC_FFLAGS_DEBUG} CACHE STRING
  	"Additional FFLAGS for Debug build" FORCE)
  set(CMAKE_Fortran_FLAGS_DEV ${FMC_FFLAGS_DEV} CACHE STRING
  	"Additional FFLAGS for Dev build" FORCE)
  # Set C Flags
  set(CMAKE_C_FLAGS ${FMC_CFLAGS} CACHE STRING 
	  "Base CFLAGS for build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE ${FMC_CFLAGS_RELEASE} CACHE STRING
	  "Additional CFLAGS for Release (optimised) build" FORCE)
  set(CMAKE_C_FLAGS_DEBUG ${FMC_CFLAGS_DEBUG} CACHE STRING
	  "Additional CFLAGS for Debug build" FORCE)
  set(CMAKE_Fortran_FLAGS_DEV ${FMC_CFLAGS_DEV} CACHE STRING
	  "Additional CFLAGS for Dev build" FORCE)
  set(FLAGS_SET 1 CACHE INTERNAL "Flags are set")
endif()


