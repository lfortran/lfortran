program gpu_metal_101
! Test: function returning allocatable array inside do concurrent
! Verifies that the Metal codegen correctly handles local allocatable
! array variables created by the subroutine_from_function pass for
! functions returning allocatable arrays.
implicit none
integer :: i
real :: a(4), r(4)
a = [1.0, 2.0, 3.0, 4.0]

do concurrent (i = 1:4)
    associate(v => get_arr(a(i)))
        r(i) = v(1)
    end associate
end do

if (abs(r(1) - 1.0) > 1e-6) error stop
if (abs(r(2) - 2.0) > 1e-6) error stop
if (abs(r(3) - 3.0) > 1e-6) error stop
if (abs(r(4) - 4.0) > 1e-6) error stop
print *, "ok"
contains
  pure function get_arr(x) result(res)
      real, intent(in) :: x
      real, allocatable :: res(:)
      allocate(res(1))
      res(1) = x
  end function
end program
