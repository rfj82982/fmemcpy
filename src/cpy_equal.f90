!!! fmemcpy.f90
!!
!!!
!!
!! SPDX-License-Identifier: BSD-3-Clause

module memcpy_equal

  use myprecision
  
  implicit none

  private

  public :: cpy_class, & 
            cpy_equal, &
            cpy_equal_brackets 

  interface cpy_equal
     module procedure cpy_equal_single
     module procedure cpy_equal_double
  end interface cpy_equal
  
  interface cpy_equal_brackets
     module procedure cpy_equal_brackets_single
     module procedure cpy_equal_brackets_double
  end interface cpy_equal_brackets

contains

  ! Simplest way of doing the copy for real 3D array
  subroutine cpy_equal_single(dst, src)

    real(stype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(stype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    
    dst = src
    
  end subroutine cpy_equal_single
  
  subroutine cpy_equal_double(dst, src)

    real(dtype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(dtype), dimension(:,:,:), intent(in) :: src    ! Source buffer

    dst = src
    
  end subroutine cpy_equal_double
  !---------------------------------------------------------------------------
  ! Simple way of doing the copy using brackets
  subroutine cpy_equal_brackets_single(dst, src)

    real(stype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(stype), dimension(:,:,:), intent(in) :: src    ! Source buffer

    dst(:,:,:) = src(:,:,:)
    
  end subroutine cpy_equal_brackets_single
  
  subroutine cpy_equal_brackets_double(dst, src)

    real(dtype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(dtype), dimension(:,:,:), intent(in) :: src    ! Source buffer

    dst(:,:,:) = src(:,:,:)
    
  end subroutine cpy_equal_brackets_double
  !---------------------------------------------------------------------------
  ! Simplest way of doing the copy using class => no type bound
  subroutine cpy_class(dst, src)

    class(*), dimension(:,:,:), intent(inout), allocatable :: dst ! Destination buffer
    class(*), dimension(:,:,:), intent(in) :: src    ! Source buffer

    dst = src(:,:,:)
    
  end subroutine cpy_class
  
end module memcpy_equal
