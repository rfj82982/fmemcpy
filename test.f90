program test_fmemcpy

  use fmemcpy
  
  implicit none

  integer, dimension(5) :: a, b
  real(kind(0.0)), dimension(5) :: c, d
  real(kind(0.0d0)), dimension(5) :: e, f
  integer :: i

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
  
end program test_fmemcpy
