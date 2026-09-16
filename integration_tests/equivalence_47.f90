program equivalence_47
   implicit none
   call asub()
   call bsub()
contains

   ! Local arrays equivalenced to elements of a COMMON block array, with the
   ! COMMON block array written last in the EQUIVALENCE statement.
   subroutine asub()
      real :: left(4), right(4), ws(8)
      common /block2_real32/ ws
      equivalence (left(1), ws(1))
      equivalence (right(1), ws(5))
      integer :: i

      do i = 1, 8
         ws(i) = real(i)
      end do

      do i = 1, 4
         print *, left(i), right(i)
         if (left(i) /= real(i)) error stop "left is not aliased to ws(1:4)"
         if (right(i) /= real(i + 4)) error stop "right is not aliased to ws(5:8)"
      end do

      left(2) = 100.0
      right(3) = 200.0

      print *, ws(2), ws(7)
      if (ws(2) /= 100.0) error stop "writing left(2) did not update ws(2)"
      if (ws(7) /= 200.0) error stop "writing right(3) did not update ws(7)"
   end subroutine asub

   ! The same, with the COMMON block array last in a three element
   ! EQUIVALENCE set.
   subroutine bsub()
      real :: u(4), v(4), ws(8)
      common /block3_real32/ ws
      equivalence (u(1), v(1), ws(5))
      integer :: i

      do i = 1, 8
         ws(i) = real(i)
      end do

      do i = 1, 4
         print *, u(i), v(i)
         if (u(i) /= real(i + 4)) error stop "u is not aliased to ws(5:8)"
         if (v(i) /= real(i + 4)) error stop "v is not aliased to ws(5:8)"
      end do

      u(1) = 50.0
      print *, ws(5), v(1)
      if (ws(5) /= 50.0) error stop "writing u(1) did not update ws(5)"
      if (v(1) /= 50.0) error stop "writing u(1) did not update v(1)"
   end subroutine bsub

end program equivalence_47
