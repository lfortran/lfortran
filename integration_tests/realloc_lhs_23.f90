module realloc_lhs_23_mod
contains
    subroutine f(xx, iord)
        integer, intent(in) :: xx(:)
        integer, intent(out) :: iord(:)
        iord = g(1.0*xx)
    end subroutine

    function g(xx) result(iord)
        real, intent(in) :: xx(:)
        integer :: iord(size(xx))
        iord = int(xx)
    end function
end module

program realloc_lhs_23
    use realloc_lhs_23_mod
    integer :: a(3), b(3)
    a = [1, 2, 3]
    call f(a, b)
    if (any(b /= a)) error stop
    print *, b
end program
