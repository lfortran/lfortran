module derived_types_167_mod
    implicit none

    type :: c_t
        character(len=8) :: name
    end type

contains

    subroutine check_host_assignment()
        type(c_t) :: host, source
        source%name = 'x'
        call assign_host()
        if (host%name /= 'x') error stop 1
    contains
        subroutine assign_host()
            host = source
        end subroutine
    end subroutine

    subroutine check_associate_assignment()
        type(c_t) :: arr(2), source
        source%name = 'y'
        associate (a => arr(2))
            a = source
        end associate
        if (arr(2)%name /= 'y') error stop 2
    end subroutine

end module

program derived_types_167
    use derived_types_167_mod, only: check_host_assignment, check_associate_assignment
    implicit none

    call check_host_assignment()
    call check_associate_assignment()
end program
