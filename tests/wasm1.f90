program wasm1
    implicit none
    contains

    function a_sqr(x) result(r)
        implicit none
        integer, intent(in):: x
        integer :: r
        r = x * x
        return
    end function

    function add(x, y) result(r)
        implicit none
        integer, intent(in):: x, y
        integer :: r
        r = x + y
        return
    end function

    function add64(x, y) result(r)
        implicit none
        integer(8), intent(in):: x, y
        integer(8) :: r
        r = x + y
        return
    end function

    function computeCircleArea(radius) result(area)
        implicit none
        integer, intent(in):: radius
        integer :: PI, area
        PI = 3
        area = PI * a_sqr(radius)
        return
    end function

    integer function my_add(a, b) result(c)
        integer, intent(in) :: a, b
        c = a + b
        return
    end function
end program
