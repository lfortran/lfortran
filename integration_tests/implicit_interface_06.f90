module implicit_interface_06_module
contains
    function f(a, b)
        integer :: a
        real :: b
        f = 1
    end function f
end

program main
    use implicit_interface_06_module
    integer :: a
    real :: i, b
    i = f(a, b)
    if (abs(i - 1) > 1e-6) error stop
    print *, i
end program main
