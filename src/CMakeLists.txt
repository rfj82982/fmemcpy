file(GLOB files_fmemcpy cmemcpy.c
	                fmemcpy.f90
			utils.f90)
include_directories(${CMAKE_SOURCE_DIR}/src)

set(SRCFILES ${files_fmemcpy} )

add_library(fmemcpy STATIC ${SRCFILES})

install(TARGETS fmemcpy
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
