program intrinsics_466
   implicit none

   call test_eoshift_rank2_dim1()
   call test_eoshift_rank2_dim2()
   call test_eoshift_repeated_array_shift()

   print *, "All tests passed."

contains

   subroutine test_eoshift_rank2_dim1()
      integer :: a(2, 3), r(2, 3)
      integer :: shifts(3)
      a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
      shifts = [1, 0, -1]
      r = eoshift(a, shifts)
      if (r(1,1) /= 2 .or. r(2,1) /= 0) error stop 1
      if (r(1,2) /= 3 .or. r(2,2) /= 4) error stop 2
      if (r(1,3) /= 0 .or. r(2,3) /= 5) error stop 3
   end subroutine

   subroutine test_eoshift_rank2_dim2()
      integer :: a(3, 2), r(3, 2)
      integer :: shifts(3)
      a = reshape([1, 2, 3, 4, 5, 6], [3, 2])
      shifts = [1, -1, 1]
      r = eoshift(a, shifts, dim=2)
      if (r(1,1) /= 4 .or. r(1,2) /= 0) error stop 4
      if (r(2,1) /= 0 .or. r(2,2) /= 2) error stop 5
      if (r(3,1) /= 6 .or. r(3,2) /= 0) error stop 6
   end subroutine

   subroutine test_eoshift_repeated_array_shift()
      integer :: a(1, 1), r(1, 1)
      integer :: shifts(1)
      a = 7
      shifts = 0
      r = eoshift(a, shifts)
      if (r(1,1) /= 7) error stop 7
      r = eoshift(a, shifts)
      if (r(1,1) /= 7) error stop 8
   end subroutine

end program
