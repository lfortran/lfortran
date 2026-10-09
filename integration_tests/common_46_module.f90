module common_46_mod
    implicit none
contains
    subroutine set_values()
        integer :: n, a
        common /common_46_blk/ n, a
        n = 3
        a = 4
    end subroutine set_values
end module common_46_mod
