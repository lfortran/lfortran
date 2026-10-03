! Instantiating a templated procedure under its own name is an error, so give
! the instance a different local name or call the templated procedure inline.
module template_instantiate_own_name_01_m
    implicit none
contains
    template function g{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        type(t) :: g
        g = x
    end function
    template subroutine s{t}(x, y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine
end module
program template_instantiate_own_name_01
    use template_instantiate_own_name_01_m
    implicit none
    integer :: y
    instantiate g {integer}, only: gi => g
    instantiate s {integer}, only: si => s
    if (gi(4) /= 4) error stop
    if (g{integer}(5) /= 5) error stop
    call si(6, y)
    if (y /= 6) error stop
    call s{integer}(7, y)
    if (y /= 7) error stop
    print *, "ok"
end program
