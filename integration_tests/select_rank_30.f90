program select_rank_30
    implicit none
    real, allocatable :: arr(:,:)
    allocate(arr(3,4))
    arr = 1.0
    call assign_by_rank(arr)
    if (abs(arr(1,1) - 2.0) > 1.0e-6) error stop
    if (abs(arr(3,4) - 2.0) > 1.0e-6) error stop
    print *, "PASS"
contains
    subroutine assign_by_rank(x)
        real, intent(inout) :: x(..)
        real, allocatable :: r(:)
        integer :: n
        n = size(x)
        allocate(r(n))
        r = 2.0
        select rank(x)
        rank(1)
            x = r
        rank(2)
            x = reshape(r, shape(x))
        rank default
            error stop "unsupported rank"
        end select
        deallocate(r)
    end subroutine
end program select_rank_30