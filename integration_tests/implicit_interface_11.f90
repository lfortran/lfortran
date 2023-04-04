module implicit_interface_11_module
implicit integer (f)
contains
    function f()
        f = 1
    end function f
end

program main
    use implicit_interface_11_module
    real :: i
    i = f()
    if (abs(i - 1) > 1e-6) error stop
    print *, i

end program main
