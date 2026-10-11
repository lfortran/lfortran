! Generic derived types (LFortran traits extension): constrained type(U)
! fields, explicit Type{...} instantiation, inference from initializer and
! structure-constructor actuals, independent type/method binders, nested
! instances, renamed re-export and outer runtime dispatch through
! class(IAverager). Standard-Fortran twin with manually concretized types:
! traits_generic_type_01_oracle.f90 (same values, same check count).
module traits_generic_type_01_contracts
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IAverager

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IAverager
        function average{INumeric :: T}(x) result(a)
            type(T), intent(in) :: x(:)
            type(T)             :: a
        end function average
    end interface IAverager
end module traits_generic_type_01_contracts

module traits_generic_type_01_leaves
    use traits_generic_type_01_contracts, only: INumeric, ISum
    implicit none
    private
    public :: SimpleSum, ScaledSum

    ! Paper-style stateless leaf with a nopass generic method.
    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, nopass :: sum => simple_sum
    end type SimpleSum

    ! A passed-object leaf whose state reaches the generic method.
    type, sealed, implements(ISum) :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass :: sum => scaled_sum
    end type ScaledSum

contains

    function simple_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum

    function scaled_sum{INumeric :: T}(self, x) result(s)
        type(ScaledSum), intent(in) :: self
        type(T),         intent(in) :: x(:)
        type(T)                     :: s
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
        s = T(self%factor) * s
    end function scaled_sum
end module traits_generic_type_01_leaves

module traits_generic_type_01_pairwise
    use traits_generic_type_01_contracts, only: ISum, INumeric
    implicit none
    private
    public :: PairwiseSum

    ! The paper's static.f90 form: sealed, adoption attribute, allocatable
    ! type(U) field, initializer in the definition and a TYPE receiver.
    type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
        private
        type(U), allocatable :: other
    contains
        initial :: init
        procedure, pass :: sum
    end type PairwiseSum

contains

    ! Receives the containing parameter U without redeclaring it.
    function init(other) result(res)
        type(U), intent(in)  :: other
        type(PairwiseSum{U}) :: res
        res%other = other
    end function init

    ! U belongs to the containing type; T is this method's own binder.
    function sum{INumeric :: T}(self, x) result(s)
        type(PairwiseSum{U}), intent(in) :: self
        type(T),              intent(in) :: x(:)
        type(T)                          :: s
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m := size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function sum
end module traits_generic_type_01_pairwise

module traits_generic_type_01_box
    use traits_generic_type_01_contracts, only: ISum, INumeric
    implicit none
    private
    public :: Box

    ! The paper's inline-listing form: a nonsealed type with public
    ! components, an implements statement repeating the parameter list, an
    ! initializer bound there and a CLASS receiver. The type binder is
    ! named T and the method binder U, the reverse of PairwiseSum.
    type :: Box{ISum :: T}
        type(T) :: item
        integer :: copies = 1
    end type Box

    implements ISum :: Box{ISum :: T}
        initial :: make
        procedure, pass :: sum => box_sum
    end implements Box

contains

    function make(item, extra) result(res)
        type(T), intent(in) :: item
        integer, intent(in) :: extra
        type(Box{T})        :: res
        res%item = item
        res%copies = extra + 1
    end function make

    function box_sum{INumeric :: U}(self, x) result(s)
        class(Box{T}), intent(in) :: self
        type(U),       intent(in) :: x(:)
        type(U)                   :: s
        s = U(self%copies) * self%item%sum(x)
    end function box_sum
end module traits_generic_type_01_box

module traits_generic_type_01_averager
    use traits_generic_type_01_contracts, only: IAverager, ISum, INumeric
    implicit none
    private
    public :: Averager

    type, sealed, implements(IAverager) :: Averager{ISum :: U}
        private
        type(U), allocatable :: drv
    contains
        initial :: init
        procedure, pass :: average
    end type Averager

contains

    function init(drv) result(res)
        type(U), intent(in) :: drv
        type(Averager{U})   :: res
        res%drv = drv
    end function init

    function average{INumeric :: T}(self, x) result(a)
        type(Averager{U}), intent(in) :: self
        type(T),           intent(in) :: x(:)
        type(T)                       :: a
        a = self%drv%sum(x) / T(size(x))
    end function average
end module traits_generic_type_01_averager

! A renamed re-export names the same generic definition.
module traits_generic_type_01_facade
    use traits_generic_type_01_pairwise, only: Pair => PairwiseSum
    implicit none
    private
    public :: Pair
end module traits_generic_type_01_facade

program traits_generic_type_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_type_01_contracts, only: IAverager
    use traits_generic_type_01_leaves, only: SimpleSum, ScaledSum
    use traits_generic_type_01_pairwise, only: PairwiseSum
    use traits_generic_type_01_box, only: Box
    use traits_generic_type_01_averager, only: Averager
    use traits_generic_type_01_facade, only: Pair
    implicit none

    type(PairwiseSum{SimpleSum}) :: plain, explicit
    type(Pair{SimpleSum}) :: renamed
    type(PairwiseSum{PairwiseSum{SimpleSum}}) :: nested
    type(Box{ScaledSum}) :: boxed
    class(IAverager), allocatable :: av
    integer, allocatable :: xi(:)
    real(real64), allocatable :: xr(:)
    integer :: n, i, base, checks

    checks = 0
    ! Initializer inference assigned to an explicitly declared instance.
    plain = PairwiseSum(SimpleSum())
    ! Explicit constructor type arguments with a keyword actual: same type.
    explicit = PairwiseSum{SimpleSum}(other = SimpleSum())
    plain = explicit
    ! The same instance through a renamed re-export.
    renamed = plain
    ! Nested inference assigned to an explicitly nested declaration.
    nested = PairwiseSum(PairwiseSum(SimpleSum()))
    ! Structure-constructor inference with keywords: no initializer matches.
    boxed = Box(item = ScaledSum(2), copies = 5)

    ! Inferred declarations of further instances.
    scaled := PairwiseSum(ScaledSum(3))
    box1 := Box(SimpleSum())
    box4 := Box(SimpleSum(), 3)
    box5 := Box{ScaledSum}(ScaledSum(5))
    pboxed := PairwiseSum(Box(ScaledSum(2), 2))
    boxpair := Box(PairwiseSum(ScaledSum(4)))

    do n = 0, 9
        xi = [(i, i = 1, n)]
        xr = [(real(i, real64), i = 1, n)]
        base = n * (n + 1) / 2
        call check_i(plain%sum(xi), base, 1)
        call check_r(plain%sum(xr), base, 2)
        call check_i(renamed%sum(xi), base, 3)
        call check_i(total_of(renamed, xi), base, 4)
        call check_i(nested%sum(xi), base, 5)
        call check_r(nested%sum(xr), base, 6)
        call check_i(scaled%sum(xi), 3 * base, 7)
        call check_r(scaled%sum(xr), 3 * base, 8)
        call check_i(box1%sum(xi), base, 9)
        call check_i(box4%sum(xi), 4 * base, 10)
        call check_i(boxed%sum(xi), 10 * base, 11)
        call check_r(boxed%sum(xr), 10 * base, 12)
        call check_i(box5%sum(xi), 5 * base, 13)
        call check_i(pboxed%sum(xi), 6 * base, 14)
        call check_r(pboxed%sum(xr), 6 * base, 15)
        call check_i(boxpair%sum(xi), 4 * base, 16)
    end do

    xi = [1, 2, 3, 4, 5]
    xr = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]
    av = Averager(drv = PairwiseSum(other = SimpleSum()))
    call check_i(av%average(xi), 3, 20)
    call check_mean(av%average(xr), 3.d0, 21)
    av = Averager(drv = boxed)
    call check_i(av%average(xi), 30, 22)
    call check_mean(av%average(xr), 30.d0, 23)
    av = Averager(Box(SimpleSum(), 1))
    call check_i(av%average(xi), 6, 24)
    call check_mean(av%average(xr), 6.d0, 25)
    av = Averager(drv = nested)
    call check_i(av%average(xi), 3, 26)
    call check_mean(av%average(xr), 3.d0, 27)

    if (checks /= 168) error stop 99
    print '(a,i0)', 'traits_generic_type_01 checks: ', checks

contains

    integer function total_of(p, x) result(r)
        type(PairwiseSum{SimpleSum}), intent(in) :: p
        integer, intent(in) :: x(:)
        r = p%sum(x)
    end function total_of

    subroutine check_i(actual, expected, code)
        integer, intent(in) :: actual, expected, code
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,i0,a,i0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_i

    subroutine check_r(actual, expected, code)
        real(real64), intent(in) :: actual
        integer, intent(in) :: expected, code
        checks = checks + 1
        if (actual /= real(expected, real64)) then
            print '(a,i0,a,g0,a,i0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_r

    subroutine check_mean(actual, expected, code)
        real(real64), intent(in) :: actual, expected
        integer, intent(in) :: code
        checks = checks + 1
        if (abs(actual - expected) > 4 * spacing(expected)) then
            print '(a,i0,a,g0,a,g0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_mean

end program traits_generic_type_01
