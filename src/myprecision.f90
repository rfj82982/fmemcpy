!!! myprecision.f90
!!
!!!
!!
!! SPDX-License-Identifier: BSD-3-Clause

module myprecision

  use, intrinsic :: iso_fortran_env, only: real32, real64
  
  implicit none

  private

  integer, parameter, public :: dtype = KIND(0._real64)
  integer, parameter, public :: stype = KIND(0._real32)

end module myprecision
