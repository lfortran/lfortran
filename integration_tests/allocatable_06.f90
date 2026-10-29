program allocatable_06
    implicit none
    call check()
    call check()
contains
    subroutine check()
        integer, allocatable :: a(:)
        if (allocated(a)) error stop 1
        allocate(a(3))
        if (.not. allocated(a)) error stop 2
        a(1) = 4
        a(2) = 5
        a(3) = 6
        if (a(2) /= 5) error stop 3
        deallocate(a)
        if (allocated(a)) error stop 4
        allocate(a(0))
        if (.not. allocated(a)) error stop 5
        deallocate(a)
        if (allocated(a)) error stop 6
    end subroutine
end program
