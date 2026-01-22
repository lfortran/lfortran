module nested_namelist_01_mod

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

end module

program nested_namelist_01
    use nested_namelist_01_mod
    implicit none
    call outer()
end program
