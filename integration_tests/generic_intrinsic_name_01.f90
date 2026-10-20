module generic_intrinsic_name_01_m
    implicit none
    integer :: calls = 0
    interface sin
        module procedure generic_intrinsic_name_01_sin
    end interface
contains
    subroutine generic_intrinsic_name_01_sin(x)
        real, intent(in) :: x
        calls = calls + 1
    end subroutine
end module

program generic_intrinsic_name_01
    use generic_intrinsic_name_01_m
    implicit none
    real :: x, y
    x = 0.5
    ! A function reference cannot resolve to the specific subroutine, so it
    ! is a reference to the intrinsic sin.
    y = sin(x)
    if (abs(y - 0.4794255) > 1e-5) error stop
    call sin(x)
    if (calls /= 1) error stop
    print *, y, calls
end program
