! An internal procedure passes an optional dummy of its host on to another
! procedure: the callee sees it present exactly when the host received it
module host_assoc_10_m
implicit none
contains

logical function is_present_scalar(k)
    integer, optional, intent(in) :: k
    is_present_scalar = present(k)
    if (present(k)) then
        if (k /= 7) error stop 1
    end if
end function is_present_scalar

subroutine check_scalar(k, expected)
    integer, optional, intent(in) :: k
    logical, intent(in) :: expected
    if (present(k) .neqv. expected) error stop 2
    if (present(k)) then
        if (k /= 7) error stop 3
    end if
end subroutine check_scalar

subroutine check_array(a, expected)
    integer, optional, intent(in) :: a(:)
    logical, intent(in) :: expected
    if (present(a) .neqv. expected) error stop 4
    if (present(a)) then
        if (size(a) /= 3) error stop 5
        if (sum(a) /= 6) error stop 6
    end if
end subroutine check_array

subroutine check_string(s, expected)
    character(len=*), optional, intent(in) :: s
    logical, intent(in) :: expected
    if (present(s) .neqv. expected) error stop 7
    if (present(s)) then
        if (s /= "abc") error stop 8
    end if
end subroutine check_string

subroutine host(expected, k, a, s)
    logical, intent(in) :: expected
    integer, optional, intent(in) :: k
    integer, optional, intent(in) :: a(:)
    character(len=*), optional, intent(in) :: s
    call inner()
    call sibling_caller()
contains
    subroutine inner()
        if (present(k) .neqv. expected) error stop 9
        if (present(a) .neqv. expected) error stop 10
        if (present(s) .neqv. expected) error stop 11
        call check_scalar(k, expected)
        call check_array(a, expected)
        call check_string(s, expected)
        if (is_present_scalar(k) .neqv. expected) error stop 12
        block
            call check_scalar(k, expected)
        end block
    end subroutine inner

    subroutine sibling(k2, expected2)
        integer, optional, intent(in) :: k2
        logical, intent(in) :: expected2
        if (present(k2) .neqv. expected2) error stop 13
        if (present(k2)) then
            if (k2 /= 7) error stop 14
        end if
    end subroutine sibling

    subroutine sibling_caller()
        call sibling(k, expected)
    end subroutine sibling_caller
end subroutine host

recursive subroutine rhost(depth, expected, k)
    integer, intent(in) :: depth
    logical, intent(in) :: expected
    integer, optional, intent(in) :: k
    call inner()
contains
    subroutine inner()
        if (present(k) .neqv. expected) error stop 15
        call check_scalar(k, expected)
        if (depth > 1) call rhost(depth - 1, expected, k)
    end subroutine inner
end subroutine rhost

end module host_assoc_10_m

program host_assoc_10
use host_assoc_10_m
implicit none
call host(.false.)
call host(.true., 7, [1, 2, 3], "abc")
call rhost(3, .false.)
call rhost(3, .true., 7)
print *, "ok"
end program host_assoc_10
