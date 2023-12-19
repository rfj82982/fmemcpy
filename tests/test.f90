program test_fmemcpy

  use iso_c_binding
  
  use fmemcpy
  
  implicit none

  call test_scalar()
  call test_1D()
  call test_2D()
  call test_simple()
contains
  
  subroutine test_scalar()

    integer :: a, b
    real(kind(0.0)) :: c, d
    real(kind(0.0d0)) :: e, f

    print *, "============================"
    print *, " TEST scalar "
    print *, "----------------------------"

    a = 1
    c = 1.0
    e = 1.0d0

    print *, "Source Data:"
    print *, "Ints: ", a
    print *, "Floats: ", c
    print *, "Doubles: ", e
  
    call memcpy(b, a, 1 * 4) ! 32 bit int
    call memcpy(d, c, 1 * 4) ! 32 bit float
    call memcpy(f, e, 1 * 8) ! 64 bit double

    print *, "Destination Data:"
    print *, "Ints: ", b
    print *, "Floats: ", d
    print *, "Doubles: ", e

  end subroutine test_scalar
  
  subroutine test_1D()

    integer, dimension(5) :: a, b
    real(kind(0.0)), dimension(5) :: c, d
    real(kind(0.0d0)), dimension(5) :: e, f
    integer :: i

    print *, "============================"
    print *, " TEST 1D "
    print *, "----------------------------"

    do i = 1, 5
       a(i) = i
       c(i) = real(i, kind(0.0))
       e(i) = real(i, kind(0.0d0))
    end do

    print *, "Source Data:"
    print *, "Ints: ", a
    print *, "Floats: ", c
    print *, "Doubles: ", e
  
    call memcpy(b, a, 5 * 4) ! 32 bit int
    call memcpy(d, c, 5 * 4) ! 32 bit float
    call memcpy(f, e, 5 * 8) ! 64 bit double

    print *, "Destination Data:"
    print *, "Ints: ", b
    print *, "Floats: ", d
    print *, "Doubles: ", e

  end subroutine test_1D
  
  subroutine test_2D()

    integer, dimension(2, 2) :: a, b
    real(kind(0.0)), dimension(2, 2) :: c, d
    real(kind(0.0d0)), dimension(2, 2) :: e, f
    integer :: i, j
    integer :: ctr
    
    print *, "============================"
    print *, " TEST 2D "
    print *, "----------------------------"

    ctr = 1
    do j = 1, 2
       do i = 1, 2
          a(i, j) = ctr
          c(i, j) = real(ctr, kind(0.0))
          e(i, j) = real(ctr, kind(0.0d0))

          ctr = ctr + 1
       end do
    end do

    print *, "Source Data:"
    print *, "Ints: ", a
    print *, "Floats: ", c
    print *, "Doubles: ", e
  
    call memcpy(b, a, 4 * 4) ! 32 bit int
    call memcpy(d, c, 4 * 4) ! 32 bit float
    call memcpy(f, e, 4 * 8) ! 64 bit double

    print *, "Destination Data:"
    print *, "Ints: ", b
    print *, "Floats: ", d
    print *, "Doubles: ", e

  end subroutine test_2D

  subroutine test_simple()

    call test_1D_simple()
    call test_2D_simple()
    
  end subroutine test_simple
  
  subroutine test_1D_simple()

    integer, dimension(5) :: a, b
    real(kind(0.0)), dimension(5) :: c, d
    real(kind(0.0d0)), dimension(5) :: e, f
    integer :: i

    print *, "============================"
    print *, " TEST 1D - simple interface "
    print *, "----------------------------"

    do i = 1, 5
       a(i) = i
       c(i) = real(i, kind(0.0))
       e(i) = real(i, kind(0.0d0))
    end do

    print *, "Source Data:"
    print *, "Ints: ", a
    print *, "Floats: ", c
    print *, "Doubles: ", e
  
    call memcpy(b, a) ! 32 bit int
    call memcpy(d, c) ! 32 bit float
    call memcpy(f, e) ! 64 bit double

    print *, "Destination Data:"
    print *, "Ints: ", b
    print *, "Floats: ", d
    print *, "Doubles: ", e

  end subroutine test_1D_simple
  
  subroutine test_2D_simple()

    integer, dimension(2, 2) :: a, b
    real(kind(0.0)), dimension(2, 2) :: c, d
    real(kind(0.0d0)), dimension(2, 2) :: e, f
    integer :: i, j
    integer :: ctr
    
    print *, "============================"
    print *, " TEST 2D - simple interface "
    print *, "----------------------------"

    ctr = 1
    do j = 1, 2
       do i = 1, 2
          a(i, j) = ctr
          c(i, j) = real(ctr, kind(0.0))
          e(i, j) = real(ctr, kind(0.0d0))

          ctr = ctr + 1
       end do
    end do

    print *, "Source Data:"
    print *, "Ints: ", a
    print *, "Floats: ", c
    print *, "Doubles: ", e
  
    call memcpy(b, a) ! 32 bit int
    call memcpy(d, c) ! 32 bit float
    call memcpy(f, e) ! 64 bit double

    print *, "Destination Data:"
    print *, "Ints: ", b
    print *, "Floats: ", d
    print *, "Doubles: ", e

  end subroutine test_2D_simple
  
end program test_fmemcpy
