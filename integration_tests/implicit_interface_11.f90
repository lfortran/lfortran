module implicit_interface_11_module
implicit integer (f)
contains
    function f()
        f = 1
    end function f
end

program main
    use implicit_interface_11_module
    integer :: i
    i = f()
    if (i /= 1) error stop
    print *, i

end program main
