program optional_14
! NULL() as an actual argument corresponding to a non-pointer non-allocatable
! optional dummy argument means the dummy argument is not present (F2018
! 15.5.2.13). For a pointer dummy argument it is a disassociated pointer.
implicit none
integer, pointer :: p => null()

if (init(null()) /= -99) error stop
if (init(42) /= 42) error stop
if (init() /= -99) error stop

call check_ptr(null(), .false.)
call check_ptr(p, .false.)

contains

    integer function init(i)
    integer, intent(in), optional :: i
    if (present(i)) then
        init = i
    else
        init = -99
    end if
    end function

    subroutine check_ptr(q, expected)
    integer, pointer, intent(in) :: q
    logical, intent(in) :: expected
    if (associated(q) .neqv. expected) error stop
    end subroutine

end program
