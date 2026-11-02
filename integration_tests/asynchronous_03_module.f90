module asynchronous_03_module
    ! Compiled separately: the ASYNCHRONOUS attribute of these module
    ! variables and dummies must survive the module file.
    implicit none
    real, asynchronous :: a1(4)
    real :: a2(4)
    asynchronous :: a2
    real :: a3(4)
contains
    subroutine fill(x, v)
        real, intent(out) :: x(:)
        asynchronous :: x
        real, intent(in) :: v
        x = v
    end subroutine fill
end module asynchronous_03_module
