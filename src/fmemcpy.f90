!!! fmemcpy.f90
!!
!!!
!!
!! SPDX-License-Identifier: BSD-3-Clause

module fmemcpy

  use iso_c_binding

  use utils
  
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

  ! Safe interface to memcpy, checks that the size of the source and destination arrays is large
  ! enough to transfer n bytes.
  !
  ! Note that the destination is inout, only the n bytes specified will be overwritten.
  subroutine memcpy(dst, src, n)

    class(*), dimension(..), intent(inout) :: dst ! Destination buffer
    class(*), dimension(..), intent(in) :: src    ! Source buffer
    integer, intent(in) :: n ! Number of bytes to copy

    call check_buffers(dst, src, n)
    call memcpy_c(dst, src, n)
    
  end subroutine memcpy

  ! Internal interface to memcpy - everything is treated as TYPE(*)/void* so no size checking can be
  ! performed.
  subroutine memcpy_c(dst, src, n)

    type(*), dimension(..), target, intent(inout) :: dst ! Destination buffer
    type(*), dimension(..), target, intent(in) :: src    ! Source buffer
    integer, intent(in) :: n ! Number of bytes to copy

    call cmemcpy(c_loc(dst), c_loc(src), int(n, c_size_t))
    
  end subroutine memcpy_c

  ! Utility subroutine to check source and destination buffers are large enough to accomodate n
  ! bytes.
  subroutine check_buffers(dst, src, n)

    class(*), dimension(..), intent(inout) :: dst ! Destination buffer
    class(*), dimension(..), intent(in) :: src    ! Source buffer
    integer, intent(in) :: n ! Number of bytes to copy

    if (.not. is_buffer_safe(dst, n)) then
       print *, "Destination array is too small to store ", n, " bytes"
       stop 1
    end if
    if (.not. is_buffer_safe(src, n)) then
       print *, "Source array is too small to store ", n, " bytes"
       stop 1
    end if
    
  end subroutine check_buffers
  
end module fmemcpy
