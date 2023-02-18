module derived_types_module_19_1
    use derived_types_module_19, only: check_proc
    implicit none
    contains
        subroutine check_here()
            call check_proc()
        end subroutine
end module

program main
    implicit none

    print *, "running derived_types_19 main program"
end program main
