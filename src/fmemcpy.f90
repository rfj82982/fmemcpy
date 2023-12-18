!!! fmemcpy.f90
!!
!!!
!!
!! SPDX-License-Identifier: BSD-3-Clause

module fmemcpy

  use iso_c_binding
  
  implicit none

  private

  public :: memcpy

  interface
     subroutine cmemcpy(dst, src, n) bind(c)
       use iso_c_binding
       type(c_ptr), value :: dst
       type(c_ptr), value :: src
       integer(c_size_t), value :: n
     end subroutine cmemcpy
  end interface
  
contains

  subroutine memcpy(dst, src, n)

    type(*), dimension(..), target :: dst             ! Destination buffer
    type(*), dimension(..), target, intent(in) :: src ! Source buffer
    integer, intent(in) :: n ! Number of bytes to copy

    ! Do some safety checking?
    
    call cmemcpy(c_loc(dst), c_loc(src), int(n, c_size_t))
    
  end subroutine memcpy
  
end module fmemcpy
