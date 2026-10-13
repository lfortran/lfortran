! Open protocol runtime generic messages beyond the paper's averagers: a
! generic procedure called from an erased body, recursion through it,
! default-initialized erased locals of a derived type, scalar and rank-2
! erased arguments, a named passed-object argument after the generic one,
! and forwarding through a generic procedure of the client.
module traits_protocol_runtime_01_contracts_m
    implicit none
    private
    public :: INumeric, IReduce

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface INumeric

    abstract interface :: IReduce
        function total{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function total
        function weighted{INumeric :: T}(x, w) result(s)
            type(T), intent(in) :: x(:)
            type(T), intent(in) :: w
            type(T)             :: s
        end function weighted
        function grid{INumeric :: T}(m) result(s)
            type(T), intent(in) :: m(:, :)
            type(T)             :: s
        end function grid
    end interface IReduce
end module traits_protocol_runtime_01_contracts_m

module traits_protocol_runtime_01_fraction_m
    use traits_protocol_runtime_01_contracts_m, only: INumeric
    implicit none
    private
    public :: Fraction

    ! The default denominator shows that erased locals are default-initialized.
    type, public, sealed :: Fraction
        integer :: num = 0
        integer :: den = 1
    end type Fraction

    implements INumeric :: Fraction
        initial :: from_integer
        procedure, static :: operator(+) => add
        procedure, static :: operator(*) => multiply
    end implements Fraction

contains

    function from_integer(n) result(res)
        integer, intent(in) :: n
        type(Fraction)      :: res
        res%num = n
        res%den = 1
    end function from_integer

    function add(lhs, rhs) result(res)
        type(Fraction), intent(in) :: lhs, rhs
        type(Fraction)             :: res
        res%num = lhs%num * rhs%den + rhs%num * lhs%den
        res%den = lhs%den * rhs%den
    end function add

    function multiply(lhs, rhs) result(res)
        type(Fraction), intent(in) :: lhs, rhs
        type(Fraction)             :: res
        res%num = lhs%num * rhs%num
        res%den = lhs%den * rhs%den
    end function multiply
end module traits_protocol_runtime_01_fraction_m

module traits_protocol_runtime_01_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_runtime_01_contracts_m, only: INumeric
    implicit none

    implements INumeric :: integer
    end implements integer

    implements INumeric :: real(real64)
    end implements real(real64)
end module traits_protocol_runtime_01_intrinsics_m

module traits_protocol_runtime_01_provider_m
    use traits_protocol_runtime_01_contracts_m, only: INumeric, IReduce
    implicit none
    private
    public :: Folder

    type, sealed, implements(IReduce) :: Folder
        integer :: calls = 0
    contains
        procedure, nopass :: total => folded_total
        procedure, pass(self) :: weighted => folded_weighted
        procedure, nopass :: grid => folded_grid
    end type Folder

contains

    ! A generic helper of the provider, instantiated once with the erased
    ! binder by each entry that calls it.
    recursive function halves{INumeric :: U}(x) result(s)
        type(U), intent(in) :: x(:)
        type(U)             :: s
        integer             :: m
        if (size(x) == 0) then
            s = U(0)
        else if (size(x) == 1) then
            s = x(1)
        else
            m = size(x) / 2
            s = halves(x(:m)) + halves(x(m+1:))
        end if
    end function halves

    function folded_total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        type(T)             :: unset
        ! An erased local starts with the type's default value.
        s = halves(x) + unset * T(0)
    end function folded_total

    function folded_weighted{INumeric :: T}(x, w, self) result(s)
        type(T),       intent(in) :: x(:)
        type(T),       intent(in) :: w
        class(Folder), intent(in) :: self
        type(T)                   :: s
        s = halves(x(size(x):1:-1)) * w + T(self%calls)
    end function folded_weighted

    function folded_grid{INumeric :: T}(m) result(s)
        type(T), intent(in) :: m(:, :)
        type(T)             :: s
        integer             :: j
        s = T(0)
        do j = 1, size(m, 2)
            s = s + halves(m(:, j))
        end do
    end function folded_grid
end module traits_protocol_runtime_01_provider_m

module traits_protocol_runtime_01_client_m
    use traits_protocol_runtime_01_contracts_m, only: INumeric, IReduce
    implicit none
    private
    public :: twice

contains

    ! A client generic procedure forwards its binder to the runtime message.
    function twice{INumeric :: V}(r, x) result(s)
        class(IReduce), intent(in) :: r
        type(V),        intent(in) :: x(:)
        type(V)                    :: s
        s = r%total(x) + r%total(x)
    end function twice
end module traits_protocol_runtime_01_client_m

program traits_protocol_runtime_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_runtime_01_contracts_m, only: IReduce
    use traits_protocol_runtime_01_fraction_m, only: Fraction
    use traits_protocol_runtime_01_intrinsics_m
    use traits_protocol_runtime_01_provider_m, only: Folder
    use traits_protocol_runtime_01_client_m, only: twice
    implicit none
    class(IReduce), allocatable :: r
    type(Fraction) :: f(4), q
    integer :: iv(6), im(2, 3), checks, i
    real(real64) :: dv(3)

    checks = 0
    r = Folder(calls = 2)
    iv = [(i, i = 1, 6)]
    im = reshape(iv, [2, 3])
    dv = [0.5_real64, 1.5_real64, 2.0_real64]
    f = [(Fraction(1, i), i = 1, 4)]

    call check(r%total(iv) == 21, 1)
    call check(r%total(iv(6:1:-2)) == 12, 2)
    call check(r%total(iv(1:0)) == 0, 3)
    call check(r%total(dv) == 4.0_real64, 4)
    q = r%total(f)
    call check(q%num == 50 .and. q%den == 24, 5)
    q = r%total(f(1:0))
    call check(q%num == 0 .and. q%den == 1, 6)
    call check(r%weighted(iv(2:4), 10) == 92, 7)
    q = r%weighted(f(2:3), Fraction(2, 1))
    call check(q%num == 22 .and. q%den == 6, 8)
    call check(r%grid(im) == 21, 9)
    call check(r%grid(im(:, 2:3)) == 18, 10)
    call check(r%grid(im(2:1:-1, 3:1:-2)) == 14, 11)
    call check(twice(r, iv(1:3)) == 12, 12)
    call check(twice(r, dv) == 8.0_real64, 13)
    q = twice(r, f(1:2))
    call check(q%num == 12 .and. q%den == 4, 14)

    print '(a,i0)', 'traits_protocol_runtime_01 checks: ', checks
    if (checks /= 14) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_protocol_runtime_01 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_protocol_runtime_01
