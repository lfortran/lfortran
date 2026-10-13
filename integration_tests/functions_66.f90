module functions_66_m
    implicit none
    integer :: nl = 4
contains
    function mk() result(r)
        character(len=nl) :: r
        r = "abcdefg"
    end function mk
    subroutine take(c, k)
        character(len=nl), intent(in) :: c
        integer, intent(out) :: k
        k = len(c)
    end subroutine take
end module functions_66_m

program functions_66
    ! Bounds of results and lengths of character dummy arguments and
    ! results that read a variable of the host or of a module
    use functions_66_m, only: mk, take
    implicit none
    integer :: n, k
    character(len=10) :: buf
    buf = mk()
    if (buf /= "abcd") error stop
    if (len(mk()) /= 4) error stop
    call take("abcd", k)
    if (k /= 4) error stop
    n = 3
    if (any(g() /= [1, 2, 3])) error stop
    if (h() /= "abc") error stop
    call s("xyz")
    n = 5
    if (size(g()) /= 5) error stop
    if (len(h()) /= 5) error stop
    print *, "ok"
contains
    function g() result(r)
        integer :: r(n)
        integer :: i
        do i = 1, n
            r(i) = i
        end do
    end function g
    function h()
        character(len=n) :: h
        h = "abcdefgh"
    end function h
    subroutine s(c)
        character(len=n), intent(in) :: c
        if (len(c) /= 3) error stop
        if (c /= "xyz") error stop
    end subroutine s
end program functions_66
