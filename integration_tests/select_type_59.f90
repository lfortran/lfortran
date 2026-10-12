module select_type_59_m
    implicit none
    type :: base_t
    end type
    type, extends(base_t) :: ext_t
        integer :: k = 7
    end type
    class(base_t), allocatable :: ib
end module select_type_59_m

program select_type_59
    use select_type_59_m
    implicit none
    allocate(ext_t :: ib)
    select type(ib)
    type is (ext_t)
        if (ib%k /= 7) error stop
    class default
        error stop
    end select
end program select_type_59