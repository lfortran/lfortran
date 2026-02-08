module modules_65_base
    implicit none

    type :: base_t
        integer :: width = 0
    end type base_t

    type :: holder_t
        class(base_t), allocatable :: item
    end type holder_t

end module modules_65_base
