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
      if (size(a) > 0) x = x + 1
    end subroutine
  end subroutine

  subroutine s_out(a)
    integer, allocatable, intent(out) :: a(:)
    allocate(a(2))
    a(1) = 3
    a(2) = 5
    call t_out(a(2))
  contains
    subroutine t_out(x)
      integer, intent(inout) :: x
      if (size(a) /= 2) error stop
      x = x + a(1)
    end subroutine
  end subroutine
end module

program nested_vars_allocatable_call
  use nested_vars_allocatable_mod
  integer, allocatable :: a(:), b(:)
  allocate(a(1))
  a = 1
  call s(a)
  if (a(1) /= 2) error stop
  call s_out(b)
  if (.not. allocated(b)) error stop
  if (size(b) /= 2) error stop
  if (b(1) /= 3 .or. b(2) /= 8) error stop
end program
