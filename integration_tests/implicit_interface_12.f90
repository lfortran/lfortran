module implicit_interface_12_module
contains
    function f()
        implicit integer(8) (f)
        f = 1
    end function f
end

program main
    use implicit_interface_12_module
    integer(8) :: i
    i = f()
    if (i /= 1) error stop
    print *, i

end program main
