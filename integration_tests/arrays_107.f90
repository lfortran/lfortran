program arrays_107
    implicit none
    real :: x(3, 1) = 0.0
    call testsub(x(1, 1))
    if (abs(x(1, 1) - 1.0/7.0) > 1e-6) error stop
    if (abs(x(2, 1) - 1.0/7.0) > 1e-6) error stop
    if (abs(x(3, 1) - 1.0/7.0) > 1e-6) error stop
contains
    subroutine testsub(x)
        real, intent(inout) :: x(3, *)
        x(:,1) = [1,1,1]/7.0
    end subroutine testsub
end program arrays_107
