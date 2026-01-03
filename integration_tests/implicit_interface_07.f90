module implicit_interface_07_module
contains
    function f()
        f = 1
    end function f
end

program main
    use implicit_interface_07_module
    integer :: j
    j = f()
    if (j /= 1) error stop
    print *, j

end program main
