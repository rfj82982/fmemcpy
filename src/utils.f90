!!! utils.f90
!!
!!!
!!
!! SPDX-License-Identifier: BSD-3-Clause

module utils

  use iso_c_binding
  
  implicit none

  private
  public :: is_buffer_safe
  public :: get_mem_size
  
contains

  ! Utility function to test if a buffer is safe to copy n bytes to/from.
  pure logical function is_buffer_safe(buf, n)

    class(*), dimension(..), intent(in) :: buf ! Buffer to test
    integer(c_size_t), intent(in) :: n         ! Number of bytes to transfer

    is_buffer_safe = (get_mem_size(buf) >= n)
    
  end function is_buffer_safe
  
  ! Utility function returning the size of its argument in bytes.
  pure integer(c_size_t) function get_mem_size(a)

    class(*), dimension(..), intent(in) :: a

    integer(c_size_t) :: nbits              ! Number of bits used by an object

    integer, parameter :: bits_per_byte = 8 ! Number of bits in a byte

    nbits = storage_size(a, c_size_t) * size(a)
    get_mem_size = nbits / bits_per_byte
    
  end function get_mem_size

end module utils
