module array_section_30_mod
   implicit none
contains
   subroutine take(a, expected_n)
      real, intent(in) :: a(:)
      integer, intent(in) :: expected_n
      if (size(a) /= expected_n) error stop
   end subroutine
   subroutine asz(y)
      real, intent(in) :: y(*)
      if (abs(y(1) - 1.0) > 1e-6) error stop
      call take(y(1:2), 2)
   end subroutine
end module array_section_30_mod

program array_section_30
   ! In-bounds sections (including empty sections, negative strides and
   ! non-default lower bounds) must not trip the runtime bounds checker.
   use array_section_30_mod
   implicit none
   real :: x(3), x2(3,3)
   real, allocatable :: xa(:), xb(:)
   integer :: i
   x = 1.0; x2 = 1.0
   allocate(xa(3)); xa = 1.0
   allocate(xb(-1:1)); xb = 1.0

   call take(x(1:3), 3)
   call take(x(:), 3)
   call take(x(2:), 2)
   call take(x(:2), 2)
   call take(x(1:3:2), 2)
   call take(x(3:1:-1), 3)
   call take(x(3:2), 0)
   call take(x(2:3:-1), 0)
   i = 2
   call take(x(i:i+1), 2)
   call take(xa(1:3), 3)
   call take(xb(-1:1), 3)
   call take(xb(-1:0), 2)
   call take(x2(2:3, 2), 2)
   call take(x2(1:3:2, 3), 2)
   call asz(x)
   print *, "ok"
end program array_section_30
