module mod_interface_20
    implicit none

    interface free_map_entry_pool
        module procedure free_map_entry_pool
    end interface free_map_entry_pool

contains

    subroutine free_map_entry_pool(pool) 
        integer, intent(inout) :: pool
        pool = 3
    end subroutine free_map_entry_pool
end module mod_interface_20

program interface_20
    use mod_interface_20
    implicit none

    integer :: i = 1
    call free_map_entry_pool(i)

    print *, i
    if (i /= 3) error stop
end program