!! SPDX-License-Identifier: BSD-3-Clause
program evaluate_fmemcpy

   use iso_c_binding
   use myprecision
   use fmemcpy
   use memcpy_equal
   use memcpy_loop
   use iso_fortran_env, only: output_unit, compiler_version, compiler_options

   implicit none

   integer :: nx = 17, ny = 13, nz = 11

   real(stype), allocatable, dimension(:, :, :) :: src_s, dst_s
   real(dtype), allocatable, dimension(:, :, :) :: src_d, dst_d

   integer :: i, j, k, m
   logical :: test_passing
   integer :: nargin, arg, FNLength, stat, DecInd, ierror, ifile
   character(len=80) :: InputFN, compiler
   character(len=32) :: fname
   character(len=3)  :: acr


   double precision :: t1, t2
   integer :: iter, niter = 10

   ! Get the input 
   ! Handle input file like a boss -- GD
   nargin = command_argument_count()
   if ((nargin == 0) .or. (nargin == 3) .or. (nargin == 4)) then
      do arg = 1, nargin
         call get_command_argument(arg, InputFN, FNLength, stat)
         read (InputFN, *, iostat=stat) DecInd
         if (arg == 1) then
            nx = DecInd
         elseif (arg == 2) then
            ny = DecInd
         elseif (arg == 3) then
            nz = DecInd
         elseif (arg == 4) then
            niter = DecInd
         end if
      end do
   else
      print *, "This Test takes no inputs, or 3 inputs as"
      print *, "  1) nx "
      print *, "  2) ny "
      print *, "  3) nz "
      print *, "or 4 inputs as"
      print *, "  1) nx "
      print *, "  2) ny "
      print *, "  3) nz "
      print *, "  4) n iterations "
      print *, "Number of inputs is not correct and the defult settings"
      print *, "will be used"
   end if

   ! Init
   allocate(src_s(nx,ny,nz))
   allocate(src_d(nx,ny,nz))
   allocate(dst_s(nx,ny,nz))
   allocate(dst_d(nx,ny,nz))
   m = 1
   do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            src_s(i, j, k) = real(m,stype)
            src_d(i, j, k) = real(m,dtype)
            m = m + 1
         end do
      end do
   end do

   compiler = compiler_version()

   ! Open the output file
   ifile = 100
   write(fname,10) 'listing_',compiler(1:3),'_',nx,'x',ny,'x',nz,'.log'
10 format(a8,a3,a,i4.4,a,i4.4,a,i4.4,a4)
   open (newunit=ifile, &
         file=fname, &
         status="replace", &
         iostat=ierror)
   print *, trim(fname)
   write(ifile,*) "Compiler version: ", compiler
   write(ifile,*) "Compiler options: ", compiler_options()

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Testing the copy routines
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Test interface with C for single 
   call cpu_time(t1)
   do iter = 1, niter
      call memcpy(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "MEMCPY SINGLE PASS time:      ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "MEMCPY SINGLE FAIL"
   end if

   ! Test interface with C for double 
   call cpu_time(t1)
   do iter = 1, niter
      call memcpy(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "MEMCPY DOUBLE PASS time:      ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "MEMCPY DOUBLE FAIL"
   end if
   !-------------------------------------------------------------------
   ! Test copy using equal for single
   dst_s = 0.0
   dst_d = 0.d0
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_equal(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "CPY EQUAL SINGLE PASS time:   ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY EQUAL SINGLE FAIL"
   end if

   ! Test copy using equal for double
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_equal(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "CPY EQUAL DOUBLE PASS time:   ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY EQUAL DOUBLE FAIL"
   end if
   !-------------------------------------------------------------------
   ! Test copy using equal for single
   dst_s = 0.0
   dst_d = 0.d0
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_equal_brackets(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "CPY BRACKET SINGLE PASS time: ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY BRACKET SINGLE FAIL"
   end if

   ! Test copy using equal for double
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_equal_brackets(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "CPY BRACKET DOUBLE PASS time: ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY BRACKET DOUBLE FAIL"
   end if
   !-------------------------------------------------------------------
   ! Test copy using equal for single
   dst_s = 0.0
   dst_d = 0.d0
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_loop3d(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "CPY LOOP3D SINGLE PASS time:  ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY LOOP3D SINGLE FAIL"
   end if

   ! Test copy using equal for double
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_loop3d(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "CPY LOOP3D DOUBLE PASS time:  ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY LOOP3D DOUBLE FAIL"
   end if
   !-------------------------------------------------------------------
   ! Test copy using equal for single
   dst_s = 0.0
   dst_d = 0.d0
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_doconcur(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "CPY DOCON SINGLE PASS time:   ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY DOCON SINGLE FAIL"
   end if

   ! Test copy using equal for double
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_doconcur(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "CPY DOCON DOUBLE PASS time:   ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY DOCON DOUBLE FAIL"
   end if
   !-------------------------------------------------------------------
   ! Test copy using equal for single
   dst_s = 0.0
   dst_d = 0.d0
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_loop2d(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "CPY LOOP2D SINGLE PASS time:  ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY LOOP2D SINGLE FAIL"
   end if

   ! Test copy using equal for double
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_loop2d(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "CPY LOOP2D DOUBLE PASS time:  ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY LOOP2D DOUBLE FAIL"
   end if

   !-------------------------------------------------------------------
   ! Test copy using equal for single
   dst_s = 0.0
   dst_d = 0.d0
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_loop1d(dst_s,src_s)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_s - src_s) == 0.0) 
   
   if (test_passing) then
      write(ifile,*) "CPY LOOP1D SINGLE PASS time:  ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY LOOP1D SINGLE FAIL"
   end if

   ! Test copy using equal for double
   call cpu_time(t1)
   do iter = 1, niter
      call cpy_loop1d(dst_d,src_d)
   enddo
   call cpu_time(t2)

   test_passing = (sum(dst_d - src_d) == 0.d0) 
   
   if (test_passing) then
      write(ifile,*) "CPY LOOP1D DOUBLE PASS time:  ", (t2-t1)/real(niter,dtype)
   else
      write(ifile,*) "CPY LOOP1D DOUBLE FAIL"
   end if
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Testing the copy routines
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   close(ifile)
   deallocate (src_s, dst_s)
   deallocate (src_d, dst_d)

end program evaluate_fmemcpy

