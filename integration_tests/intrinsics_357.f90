program intrinsics_357
    implicit none
    integer :: m, n
    real :: A(3, 4)  

    m = 3
    n = 4

    A = rand2(m, n)

contains

    function rand2(m, n) result(x)
        implicit none
        integer, intent(in) :: m, n
        real :: x(max(m, 0), max(n, 0))

        call random_number(harvest=x)
    end function rand2

end program