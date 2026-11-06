module array_size_rank_one
    implicit none
contains
    function extent(x) result(n)
        real, target, intent(in) :: x(:)
        integer(8) :: n
        n = size(x, kind=8)
    end function

    function default_extent(x) result(n)
        real, target, intent(in) :: x(-3:)
        integer :: n
        n = size(x)
    end function
end module

program check_rank_one_size
    use array_size_rank_one
    implicit none
    real :: x(-3:4), empty(0)
    x = 1.0
    if (extent(x) /= 8_8) error stop 1
    if (extent(x(-3:4:2)) /= 4_8) error stop 2
    if (extent(empty) /= 0_8) error stop 3
    if (extent(x(2:1)) /= 0_8) error stop 4
    if (extent(x(4:-3:-2)) /= 4_8) error stop 5
    if (default_extent(x) /= 8) error stop 6
    if (default_extent(empty) /= 0) error stop 7
    if (size(x(4:-3:-2), kind=8) /= 4_8) error stop 8
    if (size(x(-3:4:-1)) /= 0) error stop 9
end program
