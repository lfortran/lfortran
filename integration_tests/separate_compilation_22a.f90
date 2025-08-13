module math_separate_compilation_22
    implicit none

    interface logspace
        module subroutine logspace_sub(n)
            integer, intent(inout) :: n
        end subroutine logspace_sub
    end interface
     
end module