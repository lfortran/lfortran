module allocatable_05_storage
    implicit none
    character(:), allocatable :: s
    logical :: s_is_allocated
end module

program allocatable_05
    use allocatable_05_storage, only: renamed => s, user_flag => s_is_allocated
    implicit none
    user_flag = .true.
    if (allocated(renamed)) error stop 1
    allocate(character(0) :: renamed)
    if (.not. allocated(renamed)) error stop 2
    deallocate(renamed)
    if (.not. user_flag) error stop 3
    call local_check()
contains
    subroutine local_check()
        character(:), allocatable :: s
        logical :: s_is_allocated
        s_is_allocated = .true.
        if (allocated(s)) error stop 4
        s = "ok"
        if (.not. allocated(s)) error stop 5
        deallocate(s)
        if (.not. s_is_allocated) error stop 6
    end subroutine
end program
