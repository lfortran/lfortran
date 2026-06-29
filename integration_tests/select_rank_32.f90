program select_rank_32
    implicit none
    real, dimension(:,:), allocatable :: out2
    call extract(out2)
    if (size(out2, 1) /= 4) error stop "wrong dim 1"
    if (size(out2, 2) /= 5) error stop "wrong dim 2"
    if (abs(out2(1,1) - 1.0) > 1.0e-6) error stop "wrong val 1"
    if (abs(out2(4,5) - 20.0) > 1.0e-6) error stop "wrong val 2"
contains
    subroutine extract(output)
        real, dimension(..), allocatable, intent(out) :: output
        real, dimension(4,5) :: val
        integer :: i, j
        do j = 1, 5
            do i = 1, 4
                val(i,j) = real((j-1)*4 + i)
            end do
        end do
        select rank(output)
        rank(2)
            output = val
        rank default
            error stop "unsupported"
        end select
    end subroutine
end program select_rank_32