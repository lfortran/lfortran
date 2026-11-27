program return_main_assoc_01
    integer :: x
    x = 1
    associate(y => x)
        y = 2
        return
    end associate
end program
