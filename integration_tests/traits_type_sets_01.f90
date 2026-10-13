! LFortran traits extension test; GFortran does not accept this syntax.
! Logical members of type sets: explicit kinds, the logical(*) wildcard (the
! kinds of ISO_FORTRAN_ENV's LOGICAL_KINDS), the binary logical operators,
! logical conditions, the intrinsic LOGICAL conversion T(x), and closed
! runtime slots with logical arguments and results, also through a non-first
! passed object and keyword arguments. Integer and logical members share no
! operator, so a generic over both only stores and passes values.
module traits_type_sets_01_m
    implicit none
    private
    public :: IFold, AllOf, AnyOf, all_true, any_true, count_set, parity, &
        leading, same_as, last_of

    ! logical is logical(4).
    abstract interface :: IFlag
        logical | logical(1)
    end interface IFlag

    abstract interface :: IAnyFlag
        logical(*)
    end interface IAnyFlag

    abstract interface :: IMixed
        integer | logical
    end interface IMixed

    ! One closed runtime slot per member of each set.
    abstract interface :: IFold
        function fold{IFlag :: T}(x) result(r)
            type(T), intent(in) :: x(:)
            type(T)             :: r
        end function fold
        function count_true{IAnyFlag :: T}(x) result(n)
            type(T), intent(in) :: x(:)
            integer             :: n
        end function count_true
    end interface IFold

    type, sealed, implements(IFold) :: AllOf
    contains
        procedure, nopass :: fold => all_true
        procedure, nopass :: count_true => count_set
    end type AllOf

    ! The passed object is the second dummy argument of count_biased.
    type, sealed, implements(IFold) :: AnyOf
        integer :: bias = 0
    contains
        procedure, nopass :: fold => any_true
        procedure, pass(self) :: count_true => count_biased
    end type AnyOf

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

    function any_true{IFlag :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = T(.false.)
        do i = 1, size(x)
            r = r .or. x(i)
        end do
    end function any_true

    function count_set{IAnyFlag :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        integer             :: i
        n = 0
        do i = 1, size(x)
            if (x(i)) n = n + 1
        end do
    end function count_set

    function count_biased{IAnyFlag :: T}(x, self) result(n)
        type(T),     intent(in) :: x(:)
        type(AnyOf), intent(in) :: self
        integer                 :: n
        n = self%bias + count_set(x)
    end function count_biased

    function parity{IAnyFlag :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        integer             :: i
        r = T(.false.)
        i = 0
        do while (i < size(x))
            i = i + 1
            r = r .neqv. x(i)
        end do
    end function parity

    ! The number of leading true values.
    function leading{IAnyFlag :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        type(T)             :: more
        n = 0
        more = T(size(x) > 0)
        do while (more)
            if (x(n + 1)) then
                n = n + 1
                more = T(n < size(x))
            else
                more = T(.false.)
            end if
        end do
    end function leading

    function same_as{logical | logical(8) :: T}(a, b) result(r)
        type(T), intent(in) :: a, b
        type(T)             :: r
        r = a .eqv. b
    end function same_as

    function last_of{IMixed :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r
        type(T)             :: copy(size(x))
        copy = x
        r = copy(size(copy))
    end function last_of
end module traits_type_sets_01_m

program traits_type_sets_01
    use traits_type_sets_01_m, only: IFold, AllOf, AnyOf, all_true, any_true, &
        count_set, parity, leading, same_as, last_of
    implicit none
    logical(1) :: f1(4)
    logical(2) :: f2(3)
    logical    :: f4(5)
    logical(8) :: f8(2)
    integer    :: checks

    checks = 0
    f1 = [.true., .false., .true., .true.]
    f2 = [.false., .true., .true.]
    f4 = [.true., .true., .true., .false., .true.]
    f8 = [.true., .false.]

    call check(.not. logical(all_true(f1)))
    call check(logical(all_true(f1(3:4))))
    call check(kind(all_true(f1)) == 1)
    call check(all_true(f4(1:3)) .and. .not. all_true(f4))
    call check(kind(all_true(f4)) == kind(.true.))
    call check(all_true(f4(1:0)))
    call check(.not. logical(any_true{logical(1)}(f1(2:2))))
    call check(any_true(f4(5:1:-2)))

    call check(count_set(f1) == 3)
    call check(count_set(f2) == 2)
    call check(count_set(f4(5:1:-1)) == 4)
    call check(count_set{logical(8)}(f8) == 1)
    call check(.not. logical(parity(f2)))
    call check(logical(parity(f8)))
    call check(kind(parity(f2)) == 2)
    call check(parity(f4(1:3)))
    call check(leading(f4) == 3)
    call check(leading(f1(3:4)) == 2)
    call check(leading(f8(2:1:-1)) == 0)
    call check(leading(f2(1:0)) == 0)

    call check(.not. logical(same_as(f8(1), f8(2))))
    call check(same_as(f4(1), f4(2)))
    call check(kind(same_as(f8(1), f8(2))) == 8)

    call check(last_of([4, 5, 6]) == 6)
    call check(.not. last_of(f4(1:4)))

    call check_view(AllOf(), .false., .false., 3)
    call check_view(AnyOf(bias=10), .true., .true., 13)

    if (checks /= 35) error stop 2
    print '(a,i0,a)', 'traits_type_sets_01: ', checks, ' checks passed'

contains

    subroutine check_view(folder, fold4, fold1, count1)
        class(IFold), intent(in) :: folder
        logical,      intent(in) :: fold4, fold1
        integer,      intent(in) :: count1
        call check(folder%fold(f4) .eqv. fold4)
        call check(logical(folder%fold(f1)) .eqv. fold1)
        call check(kind(folder%fold(f1)) == 1)
        call check(folder%count_true(f1) == count1)
        call check(folder%count_true(x=f2(3:1:-1)) == count1 - 1)
    end subroutine check_view

    subroutine check(condition)
        logical, intent(in) :: condition
        checks = checks + 1
        if (.not. condition) then
            print '(a,i0,a)', 'check ', checks, ' failed'
            error stop 1
        end if
    end subroutine check
end program traits_type_sets_01
