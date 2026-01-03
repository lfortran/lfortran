module implicit_interface_10_module
    implicit none
    interface
        function fun(i)
            integer fun
        end function fun
    end interface
end module implicit_interface_10_module

program main
    use implicit_interface_10_module
end program main
