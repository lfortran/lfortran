module class_72_mod
    implicit none
    private
    public :: version_t

    type :: version_t
        integer :: major
    contains
        procedure :: greater
        generic, public :: operator(>) => greater
    end type version_t

contains

    elemental function greater(lhs, rhs) result(is_greater)
        class(version_t), intent(in) :: lhs, rhs
        logical :: is_greater
        is_greater = lhs%major > rhs%major
    end function greater

end module class_72_mod


program class_72
    use class_72_mod
    implicit none

    type(version_t), allocatable :: v1(:)
    type(version_t) :: v2

    allocate(v1(2))
    v1(1) = version_t(5)
    v2 = version_t(6)

    if (v1(1) > v2) error stop
end program class_72
