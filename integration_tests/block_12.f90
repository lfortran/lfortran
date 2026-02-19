module block_12_mod
    implicit none
    type :: legend_entry_t
        integer :: x
    end type legend_entry_t
end module block_12_mod

program block_12
    implicit none

    block
        use block_12_mod, only: legend_entry_t
        type(legend_entry_t), allocatable :: new_entries(:)
        allocate(new_entries(0))
        if (size(new_entries) /= 0) error stop
    end block

    print *, "ok"
end program block_12
