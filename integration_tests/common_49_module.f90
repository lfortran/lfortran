module common_49_mod
    implicit none
contains
    subroutine set_first()
        integer :: a
        common /common_49_blk/ a
        a = 5
    end subroutine set_first
end module common_49_mod
