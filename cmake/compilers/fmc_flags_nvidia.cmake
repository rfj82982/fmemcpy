#Compilers Flags for NVIDIA

set(FMC_FFLAGS "-cpp -Mfree -Kieee")
set(FMC_FFLAGS_RELEASE "-O3 -fast -march=native")
set(FMC_FFLAGS_DEBUG   "-O0 -g -traceback -Mbounds -Mchkptr -Ktrap=fp")
set(FMC_FFLAGS_DEV     "${FMC_FFLAGS_DEBUG}")

