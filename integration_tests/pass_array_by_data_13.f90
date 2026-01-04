program pass_array_by_data_13
    implicit none

    integer :: iseed(4)
    integer :: n
    real, allocatable :: work(:,:)

    allocate(work(3, 5))
    iseed = [1, 2, 3, 4]
    n = 15

    call slarnv(2, iseed, n, work)

    if (abs(work(1, 1) - 2.0) > 1e-6) error stop
end program pass_array_by_data_13

subroutine slarnv(idist, iseed, n, x)
    implicit none

    integer, intent(in) :: idist
    integer, intent(inout) :: iseed(*)
    integer, intent(in) :: n
    real, intent(inout) :: x(*)

    if (n > 0) x(1) = 2.0
end subroutine slarnv
