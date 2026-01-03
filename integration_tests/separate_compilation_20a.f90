module stdlib_error_separate_compilation_20
implicit none

interface
    module subroutine error_stop(code)
        integer, intent(inout) :: code
    end subroutine error_stop
end interface

end module