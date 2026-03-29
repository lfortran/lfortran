program save_14
  ! Test that save variables of derived types with default component
  ! initialization retain their values between calls (not re-initialized).
  implicit none
  type :: dt
    integer :: val = 0
  end type
  call s()
  call s()
contains
  subroutine s()
    type(dt), save :: x
    logical, save :: first = .true.
    if (first) then
      first = .false.
      x%val = 42
    end if
    if (x%val /= 42) error stop
    print *, x%val
  end subroutine
end program
