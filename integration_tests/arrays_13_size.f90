module stdlib_sorting_arrays_13_size

    implicit none

    integer, parameter :: max_merge_stack = 93

    type run_type
        integer(8) :: base = 0
        integer(8) :: len = 0
    end type run_type

end module stdlib_sorting_arrays_13_size


module stdlib_sorting_sort_index_arrays_13_size

    use stdlib_sorting_arrays_13_size
    implicit none

contains


    module subroutine sort_index( item )
        real, intent(inout) :: item
        type(run_type) :: runs(0:max_merge_stack-1)
        print *, size(runs)
        if (size(runs) /= 93) error stop
    end subroutine sort_index

end module stdlib_sorting_sort_index_arrays_13_size

program arrays_13_size
    use stdlib_sorting_sort_index_arrays_13_size
    implicit none

    real :: item
    call sort_index(item)
end program