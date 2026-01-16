program mangle_underscore_external_01
    implicit none

    interface
        subroutine foo()
        end subroutine foo
    end interface

    call foo()
end program mangle_underscore_external_01
