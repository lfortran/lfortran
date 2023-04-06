module implicit_interface_09_module
contains
    function f()
        implicit integer (f)
        f = 1
    end function f
end

program main
    use implicit_interface_09_module
    integer :: j
    j = f()
    if (j /= 1) error stop
    print *, j

end program main
