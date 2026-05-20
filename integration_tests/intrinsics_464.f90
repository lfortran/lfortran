program intrinsics_464
   implicit none

   call test_cshift_rank2_dim1()
   call test_cshift_rank2_dim2()
   call test_cshift_zero_size()
   call test_eoshift_rank2_dim1()
   call test_eoshift_rank2_dim2()

   print *, "All tests passed."

contains

   subroutine test_cshift_rank2_dim1()
      integer :: a(2, 3), r(2, 3)
      integer :: shifts(3)
      a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
      shifts = [1, 0, -1]
      r = cshift(a, shifts)
      if (r(1,1) /= 2 .or. r(2,1) /= 1) error stop 1
      if (r(1,2) /= 3 .or. r(2,2) /= 4) error stop 2
      if (r(1,3) /= 6 .or. r(2,3) /= 5) error stop 3
   end subroutine

   subroutine test_cshift_rank2_dim2()
      integer :: a(3, 2), r(3, 2)
      integer :: shifts(3)
      a = reshape([1, 2, 3, 4, 5, 6], [3, 2])
      shifts = [1, -1, 2]
      r = cshift(a, shifts, dim=2)
      if (r(1,1) /= 4 .or. r(1,2) /= 1) error stop 4
      if (r(2,1) /= 5 .or. r(2,2) /= 2) error stop 5
      if (r(3,1) /= 3 .or. r(3,2) /= 6) error stop 6
   end subroutine

   subroutine test_cshift_zero_size()
      integer :: input_array(2, 0)
      integer :: result_array(2, 0)
      integer :: shifts(0)
      result_array = cshift(input_array, shifts)
      if (size(result_array) /= 0) error stop 7
   end subroutine

   subroutine test_eoshift_rank2_dim1()
      integer :: a(2, 3), r(2, 3)
      integer :: shifts(3)
      a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
      shifts = [1, 0, -1]
      r = eoshift(a, shifts)
      if (r(1,1) /= 2 .or. r(2,1) /= 0) error stop 8
      if (r(1,2) /= 3 .or. r(2,2) /= 4) error stop 9
      if (r(1,3) /= 0 .or. r(2,3) /= 5) error stop 10
   end subroutine

   subroutine test_eoshift_rank2_dim2()
      integer :: a(3, 2), r(3, 2)
      integer :: shifts(3)
      a = reshape([1, 2, 3, 4, 5, 6], [3, 2])
      shifts = [1, -1, 1]
      r = eoshift(a, shifts, dim=2)
      if (r(1,1) /= 4 .or. r(1,2) /= 0) error stop 11
      if (r(2,1) /= 0 .or. r(2,2) /= 2) error stop 12
      if (r(3,1) /= 6 .or. r(3,2) /= 0) error stop 13
   end subroutine

end program
