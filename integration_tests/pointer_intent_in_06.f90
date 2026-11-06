! A pointer, intent(in) dummy restricts only its pointer association, not
! its target (Fortran 2023 8.5.10). It may therefore be passed to a
! non-pointer intent(inout) or intent(out) dummy, which becomes associated
! with the target, through a direct, a generic and a function call.
module pointer_intent_in_06_mod
    implicit none
    type :: counter_t
        integer :: n = 0
    end type
    interface bump_generic
        module procedure bump
        module procedure bump_counter
    end interface
contains
    subroutine bump(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine

    subroutine set_to(x, v)
        integer, intent(out) :: x
        integer, intent(in) :: v
        x = v
    end subroutine

    subroutine bump_counter(c)
        class(counter_t), intent(inout) :: c
        c%n = c%n + 1
    end subroutine

    integer function bump_and_get(x)
        integer, intent(inout) :: x
        x = x + 1
        bump_and_get = x
    end function

    subroutine use_direct(p)
        integer, pointer, intent(in) :: p
        call bump(p)
    end subroutine

    subroutine use_out(p)
        integer, pointer, intent(in) :: p
        call set_to(p, 40)
    end subroutine

    subroutine use_generic(p, c)
        integer, pointer, intent(in) :: p
        type(counter_t), pointer, intent(in) :: c
        call bump_generic(p)
        call bump_generic(c)
    end subroutine

    integer function use_function(p)
        integer, pointer, intent(in) :: p
        use_function = bump_and_get(p)
    end function

    subroutine use_array(p)
        integer, pointer, intent(in) :: p(:)
        call bump(p(2))
    end subroutine
end module

program pointer_intent_in_06
    use pointer_intent_in_06_mod
    implicit none
    integer, target :: a, arr(3)
    type(counter_t), target :: ct
    integer, pointer :: q, qa(:)
    type(counter_t), pointer :: cp

    a = 1
    q => a
    call use_direct(q)
    if (a /= 2) error stop

    call use_out(q)
    if (a /= 40) error stop

    cp => ct
    call use_generic(q, cp)
    if (a /= 41) error stop
    if (ct%n /= 1) error stop

    if (use_function(q) /= 42) error stop
    if (a /= 42) error stop

    arr = [1, 2, 3]
    qa => arr
    call use_array(qa)
    if (any(arr /= [1, 3, 3])) error stop

    print *, a, ct%n, arr
end program
