program types_16
    implicit none
    integer, parameter :: dp = kind(0.d0)

    print *, add_floats(-2.3, 5.6)
    print *, get_neg_f32()
    print *, get_pi()
    print *, get_pi_64()
    print *, computeCircleArea(5.0_dp)

    contains
    function sqr(x) result(r)
        implicit none
        real(dp), intent(in):: x
        real(dp) :: r
        r = x * x
        return
    end function

    function add_floats(x, y) result(r)
        real, intent(in) :: x, y
        real :: r
        r = x + y
        return
    end function

    function get_pi() result(r)
        real :: r
        r = 3.14
        return
    end function

    function get_pi_64() result(r)
        real(dp) :: r
        r = 3.14_dp
        return
    end function

    function get_neg_f32() result(r)
        real :: r
        r = -2.5
        return
    end function

    function computeCircleArea(radius) result(area)
        implicit none
        real(dp), intent(in):: radius
        real(dp) :: PI, area
        PI = get_pi_64()
        area = PI * sqr(radius)
        return
    end function
end program
