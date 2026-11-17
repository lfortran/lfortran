program sequence_element_02
implicit none
integer, parameter :: ip(5) = [1, 2, 3, 4, 5]
real, parameter :: rp(2, 3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
character(len=2), parameter :: cp(3) = ["ab", "cd", "ef"]
integer :: got
call sum3(ip(2), got)
if (got /= 2 + 3 + 4) error stop 1
if (sum_star(ip(3), 3) /= 3 + 4 + 5) error stop 2
if (abs(rsum(rp(2, 1)) - (2. + 3. + 4.)) > 1e-6) error stop 3
if (cat2(cp(2)) /= "cdef") error stop 4

contains

    subroutine sum3(x, s)
        integer, intent(in) :: x(3)
        integer, intent(out) :: s
        s = sum(x)
    end subroutine sum3

    integer function sum_star(x, n)
        integer, intent(in) :: n
        integer, intent(in) :: x(*)
        sum_star = sum(x(1:n))
    end function sum_star

    real function rsum(x)
        real, intent(in) :: x(3)
        rsum = sum(x)
    end function rsum

    function cat2(d) result(r)
        character(len=2), intent(in) :: d(2)
        character(len=4) :: r
        r = d(1) // d(2)
    end function cat2

end program sequence_element_02
