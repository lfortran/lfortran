module module_xx_associate_19

    type :: chaining_map_entry_pool
        type(chaining_map_entry_pool), pointer :: lastpool => null()
    end type chaining_map_entry_pool
end module module_xx_associate_19

module stdlib_hashmap_chaining_associate_19
use module_xx_associate_19

contains
    recursive subroutine free_map_entry_pool(pool)
        type(chaining_map_entry_pool), intent(inout), pointer :: pool
        if (associated(pool % lastpool)) error stop
    end subroutine free_map_entry_pool

end module stdlib_hashmap_chaining_associate_19


program associate_19
    use stdlib_hashmap_chaining_associate_19
    type(chaining_map_entry_pool), pointer :: pool
    allocate(pool)
    pool % lastpool => null()
    call free_map_entry_pool(pool)
end program associate_19

