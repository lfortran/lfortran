program gpu_metal_598
    ! Without --descriptor-index-64 a kernel computes element offsets in 32
    ! bits, so a launch must also stop when the per-thread workspace of a
    ! block-local array would have more than huge(0) elements: here 5 threads
    ! each get m = 2**29 elements, 2684354560 in all. Without the check the
    ! program would run to the end and exit 0. The check stops it before the
    ! workspace is allocated.
    implicit none
    call s(536870912)
contains
    subroutine s(m)
        integer, intent(in) :: m
        integer :: y(5)
        integer :: j
        do concurrent (j = 1:5)
            block
                logical(1) :: t(m)
                t(1) = .true.
                t(m) = .false.
                y(j) = j
                if (t(1) .and. .not. t(m)) y(j) = 10*j
            end block
        end do
        print *, y
    end subroutine
end program
