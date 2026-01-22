program nested_namelist_01
    implicit none
    call outer()
contains
    subroutine outer()
        integer :: i
        real :: x
        namelist /nml/ i, x
        i = 2
        x = 3.0
        call inner()
    contains
        subroutine inner()
            write (*, nml=nml)
        end subroutine inner
    end subroutine outer
end program nested_namelist_01
