module submodule_61_m
    implicit none
    interface
        module function s_value() result(v)
            integer :: v
        end function
        module function s_tag() result(t)
            character(len=6) :: t
        end function
    end interface
end module
