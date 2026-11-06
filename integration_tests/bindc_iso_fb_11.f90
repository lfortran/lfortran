module bindc_iso_fb_11_mod
    use iso_c_binding, only: c_int
    implicit none
contains
    subroutine f_alloc(r) bind(c)
        character(len=:), allocatable, intent(inout) :: r(:)
        allocate(character(len=3) :: r(2))
        r(1) = "abc"
        r(2) = "def"
    end subroutine

    function f_check(s) bind(c) result(n)
        character(len=*), intent(in) :: s(:)
        integer(c_int) :: n
        n = 1
        if (len(s) /= 3) return
        n = 2
        if (size(s) /= 4) return
        n = 3
        if (s(1) /= "abc" .or. s(2) /= "def" .or. s(4) /= "jkl") return
        n = 0
    end function
end module

program bindc_iso_fb_11
    use bindc_iso_fb_11_mod
    implicit none
    interface
        function c_alloc_test() bind(c) result(n)
            import :: c_int
            integer(c_int) :: n
        end function
        function c_existing_test() bind(c) result(n)
            import :: c_int
            integer(c_int) :: n
        end function
    end interface

    if (c_alloc_test() /= 0) error stop 1
    if (c_existing_test() /= 0) error stop 2
    print *, "ok"
end program
