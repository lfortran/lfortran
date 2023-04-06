module implicit_interface_13_module
contains
    function f()
        implicit complex(8) (f)
        f = (1.0, 1.0)
    end function f
end

program main
    use implicit_interface_13_module
    complex(8) :: i
    i = f()
    if (abs(real(i) - 1.0) > 1e-16) error stop
    if (abs(aimag(i) - 1.0) > 1e-16) error stop
    print *, i

end program main
