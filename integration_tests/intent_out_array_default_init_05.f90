! A fixed-size component array whose elements have default initialization:
! an intent(out) dummy and a function result get the defaults on entry.
module intent_out_array_default_init_05_m
    implicit none
    type :: inner
        real(8) :: x = 0
    end type
    type :: outer
        integer :: n = 0
        type(inner) :: d(2)
    end type
contains
    subroutine set_n(n, r)
        integer, intent(in) :: n
        type(outer), intent(out) :: r
        r%n = n
    end subroutine

    function make(n) result(r)
        integer, intent(in) :: n
        type(outer) :: r
        r%n = n
    end function
end module

program intent_out_array_default_init_05
    use intent_out_array_default_init_05_m
    implicit none
    type(outer) :: c
    c%d%x = 5
    call set_n(2, c)
    print *, c%d%x, c%n
    if (any(c%d%x /= 0) .or. c%n /= 2) error stop "intent(out)"
    c%d%x = 5
    c = make(3)
    print *, c%d%x, c%n
    if (any(c%d%x /= 0) .or. c%n /= 3) error stop "function result"
end program
