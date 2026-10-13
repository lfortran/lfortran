! Generic methods of type parameter constraints, called through the type
! parameter: forwarded to other generic procedures (also mutually recursive and
! before their definition), with explicit method type arguments by position or
! keyword, with concrete method type arguments inside the generic body, with an
! outer type parameter spelled like the method's own, with a nominally
! constrained method type parameter (pass(self) and nopass evidence), through
! an array element of the type parameter (its subscript is evaluated once), as
! a generic subroutine, and from an internal procedure. A method may have two
! type parameters or one used only by its result. GFortran does not accept this
! LFortran extension.
module traits_generic_requirement_02_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IValue, IApply, IFold
    public :: SimpleSum, ScaledSum, Value, Offset, Doubler, Folder
    public :: twice_sum, forward_sum, count_down, explicit_sum, mixed_sum
    public :: shadow_sum, apply_both, element_sum, fold_into, early_total
    public :: calls, next_index, ping, pong, inner_sum
    public :: IPair, IInline, Pairer, use_pair, unused_inline

    integer :: calls = 0

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue

    abstract interface :: IApply
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function apply
    end interface IApply

    abstract interface :: IFold
        subroutine fold{INumeric :: T}(x, total)
            type(T), intent(in) :: x(:)
            type(T), intent(inout) :: total
        end subroutine fold
    end interface IFold

    ! Two method type parameters, and one used only by the result.
    abstract interface :: IPair
        function pair{INumeric :: T, INumeric :: U}(x, y) result(r)
            type(T), intent(in) :: x
            type(U), intent(in) :: y
            integer :: r
        end function pair
        function zero{INumeric :: T}() result(z)
            type(T) :: z
        end function zero
    end interface IPair

    ! A method with an inline type set is copied with its own type set.
    abstract interface :: IInline
        function twice{integer | real(real64) :: T}(x) result(r)
            type(T), intent(in) :: x
            type(T) :: r
        end function twice
    end interface IInline

    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, nopass :: sum => simple_sum
    end type SimpleSum

    type, sealed, implements(ISum) :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass(self) :: sum => scaled_sum
    end type ScaledSum

    type :: Value
        integer :: payload = 0
    end type Value

    type :: Offset
        integer :: amount = 0
    end type Offset

    type :: Doubler
    end type Doubler

    type, sealed, implements(IFold) :: Folder
        integer :: weight = 1
    contains
        procedure, pass :: fold => folder_fold
    end type Folder

    type, sealed, implements(IPair) :: Pairer
        integer :: k = 1
    contains
        procedure, pass :: pair => pairer_pair
        procedure, nopass :: zero => pairer_zero
    end type Pairer

    implements IValue :: Value
        procedure, pass :: value => read_value
    end implements

    implements IApply :: Offset
        procedure, pass(self) :: apply => offset_apply
    end implements

    implements IApply :: Doubler
        procedure, nopass :: apply => doubler_apply
    end implements

contains

    function simple_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum

    ! The passed object is the last dummy argument.
    function scaled_sum{INumeric :: T}(x, self) result(s)
        type(T),         intent(in) :: x(:)
        type(ScaledSum), intent(in) :: self
        type(T)                     :: s
        s = T(self%factor) * simple_sum(x)
    end function scaled_sum

    integer function read_value(self) result(r)
        class(Value), intent(in) :: self
        r = self%payload
    end function read_value

    function offset_apply{IValue :: Element}(object, self) result(r)
        type(Element), intent(in) :: object
        class(Offset), intent(in) :: self
        integer :: r
        r = object%value() + self%amount
    end function offset_apply

    function doubler_apply{IValue :: Item}(object) result(r)
        type(Item), intent(in) :: object
        integer :: r
        r = 2 * object%value()
    end function doubler_apply

    subroutine folder_fold{INumeric :: T}(self, x, total)
        class(Folder), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T), intent(inout) :: total
        integer :: i
        do i = 1, size(x)
            total = total + T(self%weight) * x(i)
        end do
    end subroutine folder_fold

    function pairer_pair{INumeric :: A, INumeric :: B}(self, x, y) result(r)
        class(Pairer), intent(in) :: self
        type(A), intent(in) :: x
        type(B), intent(in) :: y
        integer :: r
        r = self%k
    end function pairer_pair

    function pairer_zero{INumeric :: T}() result(z)
        type(T) :: z
        z = T(0)
    end function pairer_zero

    ! Both method type parameters, forwarded in either order or concrete.
    function use_pair{IPair :: Pr, INumeric :: E, INumeric :: F}(p, x, y) result(r)
        type(Pr), intent(in) :: p
        type(E),  intent(in) :: x
        type(F),  intent(in) :: y
        integer :: r
        type(E) :: z
        z = p%zero{E}()
        r = p%pair(x, y) + p%pair{F, E}(y, x) + p%pair(1, 2.0_real64)
        z = z + x
    end function use_pair

    ! Never instantiated: checked at its definition only.
    function unused_inline{IInline :: Qt}(q, x) result(r)
        type(Qt), intent(in) :: q
        integer,  intent(in) :: x
        integer :: r
        r = q%twice(x) + q%twice{integer}(x)
    end function unused_inline

    integer function next_index() result(i)
        calls = calls + 1
        i = calls
    end function next_index

    ! Calls the generic before its definition in this module.
    integer function early_total(x) result(r)
        integer, intent(in) :: x(:)
        r = twice_sum(SimpleSum(), x)
    end function early_total

    function twice_sum{ISum :: Adder, INumeric :: Elem}(a, x) result(s)
        type(Adder), intent(in) :: a
        type(Elem),  intent(in) :: x(:)
        type(Elem)              :: s
        s = a%sum(x) + a%sum(x)
    end function twice_sum

    ! Forwards both binders to twice_sum and also calls the message itself.
    function forward_sum{ISum :: Summer, INumeric :: Item}(a, x) result(s)
        type(Summer), intent(in) :: a
        type(Item),   intent(in) :: x(:)
        type(Item)               :: s
        s = twice_sum(a, x) + a%sum(x)
    end function forward_sum

    ! Recursion with the same binders.
    recursive function count_down{ISum :: Adder, INumeric :: Elem}(a, x, n) result(s)
        type(Adder), intent(in) :: a
        type(Elem),  intent(in) :: x(:)
        integer,     intent(in) :: n
        type(Elem)              :: s
        if (n <= 0) then
            s = a%sum(x)
        else
            s = a%sum(x) + count_down(a, x, n - 1)
        end if
    end function count_down

    ! Explicit method type arguments, positional and by keyword.
    function explicit_sum{ISum :: Adder, INumeric :: Elem}(a, x) result(s)
        type(Adder), intent(in) :: a
        type(Elem),  intent(in) :: x(:)
        type(Elem)              :: s
        s = a%sum{Elem}(x) + a%sum{T=Elem}(x)
    end function explicit_sum

    ! Concrete method type arguments inside the generic body.
    function mixed_sum{ISum :: Adder}(a, n) result(r)
        type(Adder), intent(in) :: a
        integer,     intent(in) :: n
        real(real64)            :: r
        integer :: i
        integer, allocatable :: xi(:)
        real(real64), allocatable :: xr(:)
        allocate(xi(n), xr(n))
        do i = 1, n
            xi(i) = i
            xr(i) = real(i, real64) / 2
        end do
        r = real(a%sum(xi), real64) + a%sum{real(real64)}(xr) + a%sum{integer}(xi)
    end function mixed_sum

    ! The outer binder has the message's binder name.
    function shadow_sum{ISum :: T, INumeric :: E}(a, x) result(s)
        type(T), intent(in) :: a
        type(E), intent(in) :: x(:)
        type(E)             :: s
        s = a%sum(x)
    end function shadow_sum

    ! A message whose own binder has a nominal constraint.
    function apply_both{IApply :: Algorithm, IValue :: V}(alg, object) result(r)
        type(Algorithm), intent(in) :: alg
        type(V),   intent(in) :: object
        integer :: r
        r = alg%apply(object) + alg%apply{V}(object)
    end function apply_both

    ! An element of an array of the binder type is the receiver.
    function element_sum{ISum :: Adder, INumeric :: Elem}(as, x) result(s)
        type(Adder), intent(in) :: as(:)
        type(Elem),  intent(in) :: x(:)
        type(Elem)              :: s
        s = as(next_index())%sum(x)
    end function element_sum

    ! A generic subroutine message.
    subroutine fold_into{IFold :: Folding, INumeric :: Elem}(f, x, total)
        type(Folding), intent(in) :: f
        type(Elem), intent(in) :: x(:)
        type(Elem), intent(inout) :: total
        call f%fold(x, total)
        call f%fold{Elem}(x, total)
    end subroutine fold_into

    recursive function ping{ISum :: Adder, INumeric :: Elem}(a, x, n) result(s)
        type(Adder), intent(in) :: a
        type(Elem),  intent(in) :: x(:)
        integer,     intent(in) :: n
        type(Elem)              :: s
        s = a%sum(x)
        if (n > 0) s = s + pong(a, x, n - 1)
    end function ping

    recursive function pong{ISum :: Summer, INumeric :: Item}(b, y, n) result(s)
        type(Summer), intent(in) :: b
        type(Item),   intent(in) :: y(:)
        integer,      intent(in) :: n
        type(Item)               :: s
        s = b%sum{Item}(y) + b%sum(y)
        if (n > 0) s = s + ping(b, y, n - 1)
    end function pong

    function inner_sum{ISum :: Adder, INumeric :: Elem}(a, x) result(s)
        type(Adder), intent(in) :: a
        type(Elem),  intent(in) :: x(:)
        type(Elem)              :: s
        s = helper()
    contains
        function helper() result(r)
            type(Elem) :: r
            r = a%sum(x)
        end function helper
    end function inner_sum

end module traits_generic_requirement_02_m

program traits_generic_requirement_02
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_requirement_02_m
    implicit none
    integer :: xi(4) = [1, 2, 3, 4]
    real(real64) :: xr(3) = [0.5_real64, 1.5_real64, 2.0_real64]
    type(ScaledSum) :: scaled(3)
    integer :: ti
    real(real64) :: tr

    if (early_total(xi) /= 20) error stop 1
    if (twice_sum(ScaledSum(3), xi) /= 60) error stop 2
    if (forward_sum(SimpleSum(), xi) /= 30) error stop 3
    if (forward_sum(ScaledSum(2), xr) /= 24.0_real64) error stop 4
    if (count_down(ScaledSum(2), xi, 3) /= 80) error stop 5
    if (count_down(SimpleSum(), xr, 1) /= 8.0_real64) error stop 6
    if (explicit_sum(ScaledSum(5), xi) /= 100) error stop 7
    if (explicit_sum{SimpleSum, real(real64)}(SimpleSum(), xr) /= 8.0_real64) error stop 8
    if (mixed_sum(SimpleSum(), 4) /= 25.0_real64) error stop 9
    if (mixed_sum(ScaledSum(2), 2) /= 15.0_real64) error stop 10
    if (shadow_sum(ScaledSum(4), xi) /= 40) error stop 11
    if (shadow_sum(SimpleSum(), xr) /= 4.0_real64) error stop 12
    if (apply_both(Offset(10), Value(7)) /= 34) error stop 13
    if (apply_both(Doubler(), Value(7)) /= 28) error stop 14
    scaled(1)%factor = 1
    scaled(2)%factor = 10
    scaled(3)%factor = 100
    calls = 0
    if (element_sum(scaled, xi) /= 10) error stop 15
    if (calls /= 1) error stop 16
    if (element_sum(scaled, xr) /= 40.0_real64) error stop 17
    if (calls /= 2) error stop 18
    ti = 1
    call fold_into(Folder(2), xi, ti)
    if (ti /= 41) error stop 19
    tr = 0.0_real64
    call fold_into(Folder(1), xr, tr)
    if (tr /= 8.0_real64) error stop 20
    if (ping(SimpleSum(), xi, 2) /= 40) error stop 21
    if (ping(ScaledSum(2), xr, 3) /= 48.0_real64) error stop 22
    if (inner_sum(ScaledSum(3), xi) /= 30) error stop 23
    if (use_pair(Pairer(1), 3, 4.0_real64) /= 3) error stop 24
    if (use_pair(Pairer(2), 1.0_real64, 2.0_real64) /= 6) error stop 25
    print '(a)', 'traits_generic_requirement_02 ok'
end program traits_generic_requirement_02
