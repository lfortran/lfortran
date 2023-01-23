!> Implementation of versioning data for comparing packages
module fpm_versioning
    implicit none
    private

    public :: version_t


    type :: version_t
        private

        !> Version numbers found
        integer, allocatable :: num(:)

    contains

        generic :: operator(==) => equals
        procedure, private :: equals

        generic :: operator(/=) => not_equals
        procedure, private :: not_equals

        generic :: operator(>) => greater
        procedure, private :: greater

        generic :: operator(<) => less
        procedure, private :: less

        generic :: operator(>=) => greater_equals
        procedure, private :: greater_equals

        generic :: operator(<=) => less_equals
        procedure, private :: less_equals

    end type version_t


    !> Arbitrary internal limit of the version parser
    integer, parameter :: max_limit = 3


 contains


    !> Check to version numbers for equality
    elemental function equals(lhs, rhs) result(is_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> Version match
        logical :: is_equal

        is_equal = .not.(lhs > rhs)
        if (is_equal) then
            is_equal = .not.(rhs > lhs)
        end if

    end function equals


    !> Check two versions for inequality
    elemental function not_equals(lhs, rhs) result(not_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> Version mismatch
        logical :: not_equal

        not_equal = lhs > rhs
        if (.not.not_equal) then
            not_equal = rhs > lhs
        end if

    end function not_equals


    !> Relative comparison of two versions
    elemental function greater(lhs, rhs) result(is_greater)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is greater
        logical :: is_greater

        integer :: ii

        do ii = 1, min(size(lhs%num), size(rhs%num))
            is_greater = lhs%num(ii) > rhs%num(ii)
            if (is_greater) exit
        end do
        if (is_greater) return

        is_greater = size(lhs%num) > size(rhs%num)
        if (is_greater) then
            do ii = size(rhs%num) + 1, size(lhs%num)
                is_greater = lhs%num(ii) > 0
                if (is_greater) exit
            end do
        end if

    end function greater


    !> Relative comparison of two versions
    elemental function less(lhs, rhs) result(is_less)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is less
        logical :: is_less

        is_less = rhs > lhs

    end function less


    !> Relative comparison of two versions
    elemental function greater_equals(lhs, rhs) result(is_greater_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is greater or equal
        logical :: is_greater_equal

        is_greater_equal = .not. (rhs > lhs)

    end function greater_equals


    !> Relative comparison of two versions
    elemental function less_equals(lhs, rhs) result(is_less_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is less or equal
        logical :: is_less_equal

        is_less_equal = .not. (lhs > rhs)

    end function less_equals



 end module fpm_versioning
