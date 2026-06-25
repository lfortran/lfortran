program save_18
    implicit none

    call update(1)
    call clobber_stack()
    call update(2)

contains

    subroutine update(expected)
        integer, intent(in) :: expected
        integer, allocatable, save :: arr(:)

        if (.not. allocated(arr)) then
            allocate(arr(1))
            arr(1) = 0
        end if

        arr(1) = arr(1) + 1
        if (arr(1) /= expected) error stop "save state not preserved"
    end subroutine update

    subroutine clobber_stack()
        integer :: scratch(4096)
        integer :: i

        do i = 1, size(scratch)
            scratch(i) = i
        end do
    end subroutine clobber_stack

end program save_18
