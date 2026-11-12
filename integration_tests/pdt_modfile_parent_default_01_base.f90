module pdt_modfile_base_m
    implicit none
    type :: base_t(k)
        integer, kind :: k = 4
        integer :: parent_default = 17
    end type
    type :: base_t_4
        integer :: user_value = 99
    end type
end module
