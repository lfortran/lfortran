module lazy_allocator_m
    implicit none
contains
    subroutine conditional(needed, n, work)
        logical, intent(in) :: needed
        integer, intent(in) :: n
        real, allocatable, intent(out) :: work(:)
        integer :: i
        if (needed) then
            allocate(work(n))
            do i = 1, n
                work(i) = real(i)
            end do
        end if
    end subroutine
end module

program array_lazy_allocator
    use lazy_allocator_m
    implicit none
    real, allocatable :: work(:)
    integer :: i
    do i = 1, 3
        call conditional(.false., 6, work)
        if (allocated(work)) error stop 1
        call conditional(.true., 6, work)
        if (.not. allocated(work)) error stop 2
        if (size(work) /= 6) error stop 3
        if (sum(work) /= 21) error stop 4
        call conditional(.false., 6, work)
        if (allocated(work)) error stop 5
        call conditional(.true., 0, work)
        if (.not. allocated(work)) error stop 6
        if (size(work) /= 0) error stop 7
        deallocate(work)
        call conditional(.true., 3, work)
        if (sum(work) /= 6) error stop 8
        deallocate(work)
    end do
end program
