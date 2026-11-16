module bindc_section_01_m
    use iso_c_binding
    implicit none
    interface
        function c_sum(n, x) result(r) bind(c, name='bindc_section_01_sum')
            import :: c_int, c_long
            integer(c_int), value :: n
            integer(c_long), intent(in) :: x(*)
            integer(c_long) :: r
        end function
        subroutine c_twice(n, x) bind(c, name='bindc_section_01_double')
            import :: c_int
            integer(c_int), value :: n
            integer(c_int), intent(inout) :: x(*)
        end subroutine
    end interface
contains
    function via_assumed_shape(x) result(r)
        integer(c_long), intent(in) :: x(:)
        integer(c_long) :: r
        r = c_sum(size(x), x)
    end function
    subroutine double_assumed_shape(x)
        integer(c_int), intent(inout) :: x(:)
        call c_twice(size(x), x)
    end subroutine
end module

program bindc_section_01
    use bindc_section_01_m
    implicit none
    integer(c_long) :: a(5)
    integer(c_int) :: b(4)
    integer :: n
    a = [1_c_long, 2_c_long, 3_c_long, 4_c_long, 5_c_long]
    n = 3
    if (c_sum(3, a(3:1:-1)) /= 321) error stop 1
    if (c_sum(3, a(1:5:2)) /= 135) error stop 2
    if (via_assumed_shape(a(n:1:-1)) /= 321) error stop 3
    if (via_assumed_shape(a(1:5:2)) /= 135) error stop 4
    if (via_assumed_shape(a(5:1:-2)) /= 531) error stop 5
    b = [1_c_int, 2_c_int, 3_c_int, 4_c_int]
    call double_assumed_shape(b(4:1:-2))
    if (any(b /= [1_c_int, 4_c_int, 3_c_int, 8_c_int])) error stop 6
end program
