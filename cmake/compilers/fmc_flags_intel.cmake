# Compilers Flags for Intel

set(FMC_FFLAGS "-fpp -std08 -xHost -heaparrays -safe-cray-ptr -g -traceback")
set(FMC_FFLAGS_RELEASE "-O3 -ipo")
set(FMC_FFLAGS_DEBUG   "-g -O0 -debug extended -traceback -DDEBUG")
set(FMC_FFLAGS_DEV     "${FMC_FFLAGS_DEBUG} -warn all,noexternal")
#set(CMAKE_Fortran_FLAGS "-cpp xSSE4.2 -axAVX,CORE-AVX-I,CORE-AVX2 -ipo -fp-model fast=2 -mcmodel=large -safe-cray-ptr")
