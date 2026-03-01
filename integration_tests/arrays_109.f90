program arrays_109
    implicit none
    integer, parameter :: a(*) = pack([1, 2, 3, 4, 5], &
                                      [.true., .false., .true., .false., .true.])
    integer, parameter :: b(*) = pack([10, 20, 30], [.true., .true., .true.])
    integer, parameter :: c(*) = pack([7, 8, 9, 10], [.false., .true., .false., .true.])

    integer, parameter   :: max_ij  = 13, max_sum = 2 * max_ij ** 3
    integer              :: i, j, k
    integer, allocatable :: s2cube(:)

    if (size(a) /= 3) error stop
    if (a(1) /= 1) error stop
    if (a(2) /= 3) error stop
    if (a(3) /= 5) error stop

    if (size(b) /= 3) error stop
    if (b(1) /= 10) error stop
    if (b(2) /= 20) error stop
    if (b(3) /= 30) error stop

    if (size(c) /= 2) error stop
    if (c(1) /= 8) error stop
    if (c(2) /= 10) error stop

    s2cube = pack( [ (k, k = 1,max_sum)], &
                [ ( count( [((i**3+j**3, i = 1,j), &
                  j = 1,max_ij)] == k ) > 1, &
                  k = 1,max_sum )] )
    if (size(s2cube) /= 1) error stop
    if (s2cube(1) /= 1729) error stop
end program arrays_109