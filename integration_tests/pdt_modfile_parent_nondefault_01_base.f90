module pdt_nondefault_base_m
    implicit none
    type :: base_t(k)
        integer, kind :: k = 4
        integer :: parent_default = 17
    end type
end module
