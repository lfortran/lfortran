module submodule_58_parent
    implicit none
    interface
        module integer function value()
        end function
    end interface
end module

submodule (submodule_58_parent) submodule_58_parent
contains
    module procedure value
        value = 2
    end procedure
end submodule

program submodule_58
    use submodule_58_parent
    implicit none
    if (value() /= 2) error stop
end program
