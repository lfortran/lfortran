module nested_vars_allocatable_mod
contains
  subroutine s(a)
    integer, allocatable, intent(inout) :: a(:)
    integer :: i
    do i = 1, size(a)
      call t(a(i))
    end do
  contains
    subroutine t(x)
      integer, intent(inout) :: x
      if (size(a) > 0) x = x
    end subroutine
  end subroutine
end module

program nested_vars_allocatable_call
  use nested_vars_allocatable_mod
  integer, allocatable :: a(:)
  allocate(a(1))
  a = 1
  call s(a)
  if (a(1) /= 1) error stop
end program
