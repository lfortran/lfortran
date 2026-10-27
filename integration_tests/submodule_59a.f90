module submodule_59_m
    implicit none
    character(len=5) :: greeting = "hello"
    interface
        module function shout() result(r)
            character(len=6) :: r
        end function
    end interface
end module
