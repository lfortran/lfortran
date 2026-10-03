module elemental_24_mod
    implicit none

    type :: con_type
        integer, allocatable :: val
    end type

    interface con_type
        module procedure :: con_type_getter
    end interface

contains

    pure elemental function endswith(con, suffix) result(itis)
        type(con_type), intent(in) :: con, suffix
        logical :: itis
        itis = con%val == suffix%val
    end function

    pure elemental function con_type_getter(val) result(con)
        integer, intent(in) :: val
        type(con_type) :: con
        con%val = val
    end function

end module elemental_24_mod

program elemental_24
    use elemental_24_mod, only: con_type, endswith
    implicit none

    type(con_type) :: a, b
    type(con_type) :: pair(2)
    type(con_type), allocatable :: suffixes(:)
    logical :: r2(2)

    ! An allocatable component must be copied, not aliased, by intrinsic
    ! assignment of a derived type.
    a = con_type(5)
    b = a
    b%val = 6
    if (a%val /= 5) error stop
    if (b%val /= 6) error stop

    ! Elemental call with scalar arguments.
    if (.not. endswith(a, con_type(5))) error stop
    if (endswith(a, b)) error stop

    ! Elemental function returning a derived type, called with a rank-1 array.
    pair = con_type([1, 2])
    if (pair(1)%val /= 1) error stop
    if (pair(2)%val /= 2) error stop

    ! Rank-1 array arguments on both sides.
    r2 = endswith(pair, pair)
    if (.not. r2(1)) error stop
    if (.not. r2(2)) error stop

    suffixes = [con_type(1), con_type(2)]
    if (size(suffixes) /= 2) error stop
    if (suffixes(1)%val /= 1) error stop
    if (suffixes(2)%val /= 2) error stop

    ! Mixed scalar and array actual arguments: the scalar is broadcast.
    r2 = endswith(con_type(1), suffixes)
    if (.not. r2(1)) error stop
    if (r2(2)) error stop

    r2 = endswith(suffixes, con_type(2))
    if (r2(1)) error stop
    if (.not. r2(2)) error stop

    ! The array elements keep independent storage for their components.
    suffixes(1)%val = 9
    if (pair(1)%val /= 1) error stop
    if (suffixes(2)%val /= 2) error stop

end program elemental_24
