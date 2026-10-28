module submodule_59_parent
    implicit none
    integer, parameter :: seed = 3
    interface
        module integer function value()
        end function
        module integer function descendant_value()
        end function
    end interface
end module

module submodule_59_other
    implicit none
    integer, parameter :: seed = 5
    interface
        module integer function value()
        end function
        module integer function descendant_value()
        end function
    end interface
end module
