module character_module_array_15364_m
    implicit none
    character :: w(2)
    character(len=3) :: initialized(2) = ['ab ', 'cde']
contains
    subroutine set_arrays()
        w = ['p', 'q']
        initialized(1) = 'xy'
        initialized(2) = 'uvw'
    end subroutine

    subroutine check_arrays()
        if (w(1) /= 'r' .or. w(2) /= 's') error stop 'module cannot see w'
        if (initialized(1) /= 'one' .or. initialized(2) /= 'two') &
            error stop 'module cannot see initialized'
    end subroutine
end module
