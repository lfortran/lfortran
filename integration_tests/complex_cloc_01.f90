module complex_cloc_01_module
    implicit none
contains
    impure elemental function cmag(x, y) result(r)
        real, intent(in), target :: x, y
        real :: r
        r = sqrt(x**2 + y**2)
    end function
end module complex_cloc_01_module

program complex_cloc_01
    use complex_cloc_01_module
    implicit none
    complex, target :: z(3)
    real :: q(3)

    z%re = [1.0, 2.0, 3.0]
    z%im = [0.0, 0.0, 0.0]

    q = cmag(z%re, z%im)

    if (abs(q(1) - 1.0) > 1e-6) error stop
    if (abs(q(2) - 2.0) > 1e-6) error stop
    if (abs(q(3) - 3.0) > 1e-6) error stop

    ! Non-trivial case: 3-4-5 right triangle
    z(1) = (3.0, 4.0)
    q(1) = cmag(z(1)%re, z(1)%im)
    if (abs(q(1) - 5.0) > 1e-6) error stop

end program complex_cloc_01
