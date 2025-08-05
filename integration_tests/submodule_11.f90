module math_submodule_11
    implicit none

    interface logspace
        module subroutine logspace_sub(n)
            integer, intent(inout) :: n
        end subroutine logspace_sub
    end interface
     
end module

submodule (math_submodule_11) math_submodule_11_logspace
    implicit none

contains

    module procedure logspace_sub
      n = 3
    end procedure

end submodule

program submodule_11
    use math_submodule_11
    implicit none

    integer :: n = 2
    call logspace(n)

    print *, n
    if (n /= 3) error stop
end program