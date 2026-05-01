module select_rank_31_mod
    implicit none
contains
    subroutine extract(src, output)
        real, intent(in) :: src(:,:)
        real, dimension(..), allocatable, intent(inout) :: output

        select rank(output)
        rank(1)
            output = reshape(src, [size(src)])
        rank default
            error stop "unsupported rank"
        end select
    end subroutine extract
end module select_rank_31_mod

program select_rank_31
    use select_rank_31_mod
    implicit none
    real, allocatable :: y(:)
    real, allocatable :: src(:,:)
    integer :: i, j

    allocate(src(2,3))
    do j = 1, 3
        do i = 1, 2
            src(i, j) = real(i + j)
        end do
    end do

    allocate(y(6))
    y = 0.0

    call extract(src, y)
    if (size(y) /= 6) error stop "wrong size"
    if (abs(sum(y) - sum(src)) > 1.0e-5) error stop "wrong sum"
    print *, "PASS"
end program select_rank_31
