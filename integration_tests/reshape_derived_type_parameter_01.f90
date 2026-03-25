program reshape_derived_type_parameter_01
    implicit none

    type :: pair
        integer :: fst
        real :: snd
    end type

    type(pair), parameter :: values(2, 4) = reshape( &
        [ pair(1, 53.), pair(3, 47.), pair(5, 43.), pair(7, 41.), &
          pair(11, 37.), pair(13, 31.), pair(17, 29.), pair(19, 23.) ], &
        [2, 4] )

    if (values(1, 1)%fst /= 1) error stop
    if (values(2, 1)%fst /= 3) error stop
    if (values(1, 2)%fst /= 5) error stop
    if (abs(values(2, 4)%snd - 23.0) > 1e-6) error stop
end program reshape_derived_type_parameter_01
