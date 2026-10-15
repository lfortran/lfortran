module allocated_06_m
  implicit none
contains
  subroutine flag(is_allocated, r)
    logical, intent(in) :: is_allocated
    integer, intent(out) :: r
    r = 0
    if (is_allocated) r = 1
  end subroutine

  integer function flag_f(is_allocated) result(r)
    logical, intent(in) :: is_allocated
    r = 1
    if (is_allocated) return
    r = 0
  end function
end module

program allocated_06
  ! `allocated(a)` is only asked inside the arguments of a call, so with
  ! --fast `a` must stay allocatable.
  use allocated_06_m
  implicit none
  real, allocatable :: a(:), b(:)
  integer :: r
  allocate(a(4))
  a = 0
  call flag(allocated(a), r)
  print *, r
  if (r /= 1) error stop
  allocate(b(4))
  b = 0
  r = flag_f(allocated(b))
  print *, r
  if (r /= 1) error stop
end program
