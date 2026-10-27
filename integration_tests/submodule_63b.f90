module submodule_63_m
    implicit none
    interface
        module function s1_value() result(v)
            integer :: v
        end function
        module function s2_value() result(v)
            integer :: v
        end function
    end interface
end module
