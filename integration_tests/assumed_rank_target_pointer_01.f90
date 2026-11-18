! A nonpointer TARGET assumed-rank actual passed to a POINTER, INTENT(IN)
! assumed-rank dummy, also when both are OPTIONAL, in a subroutine call and in
! a function call (#17257)
module assumed_rank_target_pointer_01_mod
    implicit none
contains
    real function total(w)
        real, pointer, intent(in) :: w(..)
        total = -1.0
        select rank (w)
        rank (3)
            if (any(shape(w) /= [2, 5, 3])) error stop 1
            total = sum(w)
        end select
    end function

    subroutine count_dims(n, v)
        integer, intent(out) :: n
        real, pointer, intent(in), optional :: v(..)
        n = -1
        if (present(v)) n = rank(v)
    end subroutine

    integer function dims_or_minus_one(u)
        real, pointer, intent(in), optional :: u(..)
        dims_or_minus_one = -1
        if (present(u)) dims_or_minus_one = rank(u)
    end function

    real function relay(buf)
        real, target, intent(in) :: buf(..)
        relay = total(buf)
    end function

    subroutine relay_optional(n, buf)
        integer, intent(out) :: n
        real, target, intent(in), optional :: buf(..)
        call count_dims(n, buf)
    end subroutine

    integer function relay_optional_function(buf)
        real, target, intent(in), optional :: buf(..)
        relay_optional_function = dims_or_minus_one(buf)
    end function
end module

program assumed_rank_target_pointer_01
    use assumed_rank_target_pointer_01_mod
    implicit none
    real, target :: cube(2, 5, 3)
    integer :: i, n
    cube = reshape([(real(i), i = 1, 30)], [2, 5, 3])
    if (abs(relay(cube) - 465.0) > 1e-4) error stop 2
    call relay_optional(n, cube)
    if (n /= 3) error stop 3
    call relay_optional(n)
    if (n /= -1) error stop 4
    if (relay_optional_function(cube) /= 3) error stop 5
    if (relay_optional_function() /= -1) error stop 6
    print *, "OK"
end program
