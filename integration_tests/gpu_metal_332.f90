module gpu_metal_332_first
contains
    pure real function f(x)
        real, intent(in) :: x
        f = x + 3.0
    end function
end module

module gpu_metal_332_second
contains
    pure real function f(x)
        real, intent(in) :: x
        f = 10.0*x
    end function
end module

program gpu_metal_332
    use gpu_metal_332_first, only: first => f
    use gpu_metal_332_second, only: second => f
    implicit none
    real :: result(4)
    integer :: i
    do concurrent (i = 1:4)
        result(i) = first(real(i)) + second(real(i))
    end do
    do i = 1, 4
        if (abs(result(i) - (11*i + 3)) > 1.e-6) error stop 1
    end do
end program
