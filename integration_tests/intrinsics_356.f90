program intrinsics_356
    implicit none
    integer :: m
    real :: A(3)  

    m = 3
    A = rand1(m)
    
contains

    function rand1(m) result(x)
        implicit none
        integer, intent(in) :: m
        real :: x(max(m, 0))

        call random_number(harvest=x)
    end function

end program