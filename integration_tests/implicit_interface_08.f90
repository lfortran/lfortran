module implicit_interface_08_module
implicit none
contains
    function f()
        implicit double precision (f)
        f = 1.00
    end function f
end

program main
    use implicit_interface_08_module
    double precision :: i
    i = f()
    if (abs(i - 1) > 1e-6) error stop
    print *, i

end program main
