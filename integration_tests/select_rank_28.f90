program select_rank_28
    implicit none
    real :: a(2, 3)
    integer :: i, j
    a = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
    call assign_reshaped(a)
    if (abs(a(1,1) - 10.0) > 0.001) error stop
    if (abs(a(2,1) - 20.0) > 0.001) error stop
    if (abs(a(1,2) - 30.0) > 0.001) error stop
    if (abs(a(2,2) - 40.0) > 0.001) error stop
    if (abs(a(1,3) - 50.0) > 0.001) error stop
    if (abs(a(2,3) - 60.0) > 0.001) error stop
    print *, "PASSED"
contains
    subroutine assign_reshaped(x)
        real, dimension(..), intent(inout) :: x
        select rank(x)
        rank(2)
            x = reshape([10.0, 20.0, 30.0, 40.0, 50.0, 60.0], [2, 3])
        end select
    end subroutine
end program
