program parameter_reshape_derived_type_01
    implicit none
    type :: pair
        integer :: i
        real :: r
    end type
    type(pair), parameter :: values(2, 2) = reshape( &
        [ pair(1, 53.), pair(3, 47.), pair(5, 43.), pair(7, 41.) ], &
        [2, 2])
    if (values(1, 1)%i /= 1) error stop
    if (values(1, 1)%r /= 53.) error stop
    if (values(2, 2)%i /= 7) error stop
    if (values(2, 2)%r /= 41.) error stop
end program
