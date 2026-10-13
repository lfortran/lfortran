! Type sets with logical and character members, compiled separately from
! traits_type_sets_03.f90: their members, the witnesses of their operations
! and generics constrained by them are read back from the module file.
module traits_type_sets_03_sets
    implicit none
    private
    public :: IFlag, ICode, all_true, count_set, smallest, doubled, both

    abstract interface :: IFlag
        logical(*)
    end interface IFlag

    abstract interface :: ICode
        character(len=3) | character(len=5)
    end interface ICode

contains

    function all_true{IFlag :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = T(.true.)
        do i = 1, size(x)
            r = r .and. x(i)
        end do
    end function all_true

    function count_set{IFlag :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        integer             :: i
        n = 0
        do i = 1, size(x)
            if (x(i) .neqv. T(.false.)) n = n + 1
        end do
    end function count_set

    function smallest{ICode :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = x(1)
        do i = 2, size(x)
            if (x(i) < r) r = x(i)
        end do
    end function smallest

    function doubled{character(len=3) :: T}(x) result(r)
        type(T), intent(in) :: x
        character(len=6)    :: r
        r = x // x
    end function doubled

    function both{logical | logical(1) :: T}(a, b) result(r)
        type(T), intent(in) :: a, b
        type(T)             :: r
        r = a .and. b
    end function both
end module traits_type_sets_03_sets
