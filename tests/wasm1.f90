program wasm1
    implicit none

    print *, sqr(5)
    print *, add(5, 4)
    print *, add64(4_8, 5_8)
    print *, computeCircleArea(5)
    print *, my_add(5, 4)

    contains

    function sqr(x) result(r)
        implicit none
        integer, intent(in):: x
        integer :: r
        r = x * x
    end function

    function add(x, y) result(r)
        implicit none
        integer, intent(in):: x, y
        integer :: r
        r = x + y
    end function

    function add64(x, y) result(r)
        implicit none
        integer(8), intent(in):: x, y
        integer(8) :: r
        r = x + y
    end function

    function computeCircleArea(radius) result(area)
        implicit none
        integer, intent(in):: radius
        integer :: PI, area
        PI = 3
        area = PI * sqr(radius)
    end function

    integer function my_add(a, b) result(c)
        integer, intent(in) :: a, b
        c = a + b
    end function
end program
