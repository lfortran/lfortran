program gpu_metal_331
    implicit none
    integer :: values(4), i
    call fill(values)
    do i = 1, 4
        if (values(i) /= i*(i+1)) error stop 1
    end do
contains
    subroutine fill(values)
        integer, intent(out) :: values(4)
        integer :: i
        do concurrent (i=1:4)
            block
                integer, allocatable :: work(:)
                allocate(work(i+1))
                work = i
                values(i) = sum(work)
            end block
        end do
    end subroutine
end program
