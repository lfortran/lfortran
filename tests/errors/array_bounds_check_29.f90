program array_bounds_check_28
    implicit none

    type :: t
        integer, allocatable :: d(:)
    end type t

    type(t) :: r(1)
    r(1)%d = 5
end program array_bounds_check_28
