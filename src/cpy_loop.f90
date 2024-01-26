!!! fmemcpy.f90
!!
!!!
!!
!! SPDX-License-Identifier: BSD-3-Clause

module memcpy_loop

  use myprecision
  
  implicit none

  private

  public :: cpy_loop3d, & 
            cpy_doconcur, &
            cpy_loop2d, &
            cpy_loop1d

  interface cpy_loop3d
     module procedure cpy_loop3d_single
     module procedure cpy_loop3d_double
  end interface cpy_loop3d
  
  interface cpy_doconcur
     module procedure cpy_doconcur_single
     module procedure cpy_doconcur_double
  end interface cpy_doconcur

  interface cpy_loop2d
     module procedure cpy_loop2d_single
     module procedure cpy_loop2d_double
  end interface cpy_loop2d

  interface cpy_loop1d
     module procedure cpy_loop1d_single_short
     module procedure cpy_loop1d_single_long
     module procedure cpy_loop1d_double_short
     module procedure cpy_loop1d_double_long
  end interface cpy_loop1d

contains
  !---------------------------------------------------------------------------
  ! 3D loop do do the copy
  subroutine cpy_loop3d_single(dst, src)

    real(stype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(stype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    
    integer :: s1, s2, s3
    integer :: i1, i2, i3
    
    s1 = SIZE(src, 1)
    s2 = SIZE(src, 2)
    s3 = SIZE(src, 3)

    do i3=1, s3
      do i2=1, s2
        do i1=1, s1
          dst(i1,i2,i3) = src(i1,i2,i3)
        enddo
      enddo
    enddo
    
  end subroutine cpy_loop3d_single
  
  subroutine cpy_loop3d_double(dst, src)

    real(dtype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(dtype), dimension(:,:,:), intent(in) :: src    ! Source buffer

    integer :: s1, s2, s3
    integer :: i1, i2, i3
    
    s1 = SIZE(src, 1)
    s2 = SIZE(src, 2)
    s3 = SIZE(src, 3)

    do i3= 1, s3
      do i2= 1, s2
        do i1= 1, s1
          dst(i1,i2,i3) = src(i1,i2,i3)
        enddo
      enddo
    enddo
    
  end subroutine cpy_loop3d_double
  !---------------------------------------------------------------------------
  ! Do concurrent loop to do the copy
  subroutine cpy_doconcur_single(dst, src)

    real(stype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(stype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    
    integer :: s1, s2, s3
    integer :: i1, i2, i3
    
    s1 = SIZE(src, 1)
    s2 = SIZE(src, 2)
    s3 = SIZE(src, 3)

    do concurrent (i3=1:s3, i2=1:s2, i1=1:s1)
      dst(i1,i2,i3) = src(i1,i2,i3)
    enddo
    
  end subroutine cpy_doconcur_single
  
  subroutine cpy_doconcur_double(dst, src)

    real(dtype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(dtype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    
    integer :: s1, s2, s3
    integer :: i1, i2, i3
    
    s1 = SIZE(src, 1)
    s2 = SIZE(src, 2)
    s3 = SIZE(src, 3)

    do concurrent (i3=1:s3, i2=1:s2, i1=1:s1)
      dst(i1,i2,i3) = src(i1,i2,i3)
    enddo
    
  end subroutine cpy_doconcur_double
  
  !---------------------------------------------------------------------------
  ! 3D loop do do the copy
  subroutine cpy_loop2d_single(dst, src)

    real(stype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(stype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    
    integer :: s2, s3
    integer :: i2, i3
    
    s2 = SIZE(src, 2)
    s3 = SIZE(src, 3)

    do i3=1, s3
      do i2=1, s2
        dst(:,i2,i3) = src(:,i2,i3)
      enddo
    enddo
    
  end subroutine cpy_loop2d_single
  
  subroutine cpy_loop2d_double(dst, src)

    real(dtype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(dtype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    
    integer :: s2, s3
    integer :: i2, i3
    
    s2 = SIZE(src, 2)
    s3 = SIZE(src, 3)

    do i3=1, s3
      do i2=1, s2
        dst(:,i2,i3) = src(:,i2,i3)
      enddo
    enddo
    
  end subroutine cpy_loop2d_double

  !---------------------------------------------------------------------------
  ! 1D loop do do the copy
  subroutine cpy_loop1d_single_short(dst, src)
    implicit none

    real(stype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(stype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    integer :: nsize

    nsize = size(src)

    call cpy_loop1d_single_long(dst, src, nsize) 
  
  end subroutine cpy_loop1d_single_short
  
  subroutine cpy_loop1d_single_long(dst, src, n)
    implicit none

    real(stype), intent(out):: dst(*) ! Destination buffer
    real(stype), intent(in) :: src(*) ! Source buffer
    integer, intent(in) :: n

    integer, parameter :: ni = 16
    integer :: b, i, ng

    ng = n/ni

    do b = 1, ng
       !$omp simd
       do i = 1, ni
           dst((b-1)*ni+i) = src((b-1)*ni+i)
       end do
       !$omp end simd
    end do
    
    if ((ng-1)*ni < n) then
      dst((ng-1)*ni+1:n) = src((ng-1)*ni+1:n)
    endif

  end subroutine cpy_loop1d_single_long

  subroutine cpy_loop1d_double_short(dst, src)
    implicit none

    real(dtype), dimension(:,:,:), intent(out):: dst ! Destination buffer
    real(dtype), dimension(:,:,:), intent(in) :: src    ! Source buffer
    integer :: nsize

    nsize = size(src)

    call cpy_loop1d_double_long(dst, src, nsize) 
  
  end subroutine cpy_loop1d_double_short
  
  subroutine cpy_loop1d_double_long(dst, src, n)
    implicit none

    real(dtype), intent(out):: dst(*) ! Destination buffer
    real(dtype), intent(in) :: src(*) ! Source buffer
    integer, intent(in) :: n

    integer, parameter :: ni = 8
    integer :: b, i, ng

    ng = n/ni

    do b = 1, ng
       !$omp simd
       do i = 1, ni
           dst((b-1)*ni+i) = src((b-1)*ni+i)
       end do
       !$omp end simd
    end do

    if ((ng-1)*ni < n) then 
      dst((ng-1)*ni+1:n) = src((ng-1)*ni+1:n)
    endif

  end subroutine cpy_loop1d_double_long

end module memcpy_loop
