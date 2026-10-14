! Character arguments of open protocol runtime generic messages: their
! hidden lengths follow the binders' dictionaries in every slot, entry,
! witness and call, for implementations whose passed object is first and
! unused or named and used.
module traits_protocol_runtime_02_contracts_m
    implicit none
    private
    public :: INumeric, ILabelled

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: ILabelled
        function total{INumeric :: T}(self, label, x, suffix) result(s)
            type(itself), intent(in), pass :: self
            character(len=*), intent(in) :: label
            type(T),          intent(in) :: x(:)
            character(len=*), intent(in) :: suffix
            type(T)                      :: s
        end function total
    end interface ILabelled
end module traits_protocol_runtime_02_contracts_m

module traits_protocol_runtime_02_types_m
    use traits_protocol_runtime_02_contracts_m, only: INumeric
    implicit none
    private
    public :: Tally

    type, public, sealed :: Tally
        integer :: n = 0
    end type Tally

    implements INumeric :: Tally
        initial :: tally_of
        procedure, static :: operator(+) => joined
    end implements Tally

    implements INumeric :: integer
    end implements integer

contains

    function tally_of(n) result(res)
        integer, intent(in) :: n
        type(Tally)         :: res
        res%n = n
    end function tally_of

    function joined(lhs, rhs) result(res)
        type(Tally), intent(in) :: lhs, rhs
        type(Tally)             :: res
        res%n = lhs%n + rhs%n
    end function joined
end module traits_protocol_runtime_02_types_m

module traits_protocol_runtime_02_provider_m
    use traits_protocol_runtime_02_contracts_m, only: INumeric, ILabelled
    implicit none
    private
    public :: Plain, Weighted

    type, sealed, implements(ILabelled) :: Plain
    contains
        procedure, pass :: total => plain_total
    end type Plain

    type, sealed, implements(ILabelled) :: Weighted
        integer :: weight = 1
    contains
        procedure, pass(self) :: total => weighted_total
    end type Weighted

contains

    function plain_total{INumeric :: T}(self, label, x, suffix) result(s)
        type(Plain), intent(in) :: self
        character(len=*), intent(in) :: label
        type(T),          intent(in) :: x(:)
        character(len=*), intent(in) :: suffix
        type(T)                      :: s
        integer                      :: i
        s = T(len(label) + 10 * len(suffix))
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function plain_total

    function weighted_total{INumeric :: T}(label, x, suffix, self) result(s)
        character(len=*), intent(in) :: label
        type(T),          intent(in) :: x(:)
        character(len=*), intent(in) :: suffix
        class(Weighted),  intent(in) :: self
        type(T)                      :: s
        integer                      :: i
        s = T(self%weight * len(label // suffix))
        do i = size(x), 1, -1
            s = s + x(i)
        end do
    end function weighted_total
end module traits_protocol_runtime_02_provider_m

program traits_protocol_runtime_02
    use traits_protocol_runtime_02_contracts_m, only: ILabelled
    ! Unrestricted: its integer acknowledgement has no name to import.
    use traits_protocol_runtime_02_types_m
    use traits_protocol_runtime_02_provider_m, only: Plain, Weighted
    implicit none
    class(ILabelled), allocatable :: p
    type(Tally) :: t(3), r
    character(len=:), allocatable :: name
    integer :: checks

    checks = 0
    t = [Tally(1), Tally(2), Tally(4)]
    name = "abcde"

    p = Plain()
    call check(p%total("abcd", [1, 2, 3], "x") == 20, 1)
    call check(p%total(name, [integer ::], "") == 5, 2)
    r = p%total(name(2:3), t(3:1:-1), "yz")
    call check(r%n == 29, 3)

    p = Weighted(weight = 3)
    call check(p%total("ab", [5, 6], "c") == 20, 4)
    r = p%total(name, t, "")
    call check(r%n == 22, 5)

    print '(a,i0)', 'traits_protocol_runtime_02 checks: ', checks
    if (checks /= 5) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_protocol_runtime_02 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_protocol_runtime_02
