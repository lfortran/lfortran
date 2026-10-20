! LFortran traits extension test; GFortran does not accept this syntax.
! Trait messages whose arguments are dummy procedures and procedure pointers
! in every form: a dummy procedure declared with procedure(iface), an
! optional dummy procedure, a dummy procedure of a subroutine interface, a
! procedure pointer with intent(out) set by the message, one with
! intent(inout) and a function message whose result depends on the
! procedure it is given. The interfaces take borrowed views of a trait and
! return `class(*), allocatable` or integer results. Each message is sent
! through owners, borrowed views and trait pointers.
! traits_runtime_procedure_argument_02_oracle.f90 is the standard-Fortran
! counterpart.
module traits_runtime_procedure_argument_02_m
    implicit none
    abstract interface :: ISource
        integer function value(self)
            type(itself), pass, intent(in) :: self
        end function value
    end interface

    abstract interface
        function imake(s) result(res)
            import; implicit none
            class(ISource), intent(in) :: s
            class(*), allocatable :: res
        end function imake
        integer function itransform(n)
            integer, intent(in) :: n
        end function itransform
        subroutine ivisit(s, total)
            import; implicit none
            class(ISource), intent(in) :: s
            integer, intent(inout) :: total
        end subroutine ivisit
    end interface

    abstract interface, extends(ISource) :: IStore
        ! A dummy procedure, called by the message.
        function make(self, factory) result(res)
            import; implicit none
            type(itself), pass, intent(in) :: self
            procedure(imake) :: factory
            class(*), allocatable :: res
        end function make
        ! An optional dummy procedure.
        integer function transformed(self, f)
            import; implicit none
            type(itself), pass, intent(in) :: self
            procedure(itransform), optional :: f
        end function transformed
        ! A dummy procedure of a subroutine interface.
        subroutine visit(self, v, total)
            import; implicit none
            type(itself), pass, intent(in) :: self
            procedure(ivisit) :: v
            integer, intent(inout) :: total
        end subroutine visit
        ! Procedure pointers set and updated by the message.
        subroutine choose(self, f)
            import; implicit none
            type(itself), pass, intent(in) :: self
            procedure(itransform), pointer, intent(out) :: f
        end subroutine choose
        subroutine swap(self, f)
            import; implicit none
            type(itself), pass, intent(inout) :: self
            procedure(itransform), pointer, intent(inout) :: f
        end subroutine swap
    end interface

    type :: Pair
        integer :: a = 0, b = 0
    end type Pair

    type, implements(IStore) :: Store
        integer :: n = 0
        procedure(itransform), pointer, nopass :: saved => null()
    contains
        procedure :: value => store_value
        procedure :: make => store_make
        procedure :: transformed => store_transformed
        procedure :: visit => store_visit
        procedure :: choose => store_choose
        procedure :: swap => store_swap
    end type Store
contains
    integer function store_value(self)
        class(Store), intent(in) :: self
        store_value = self%n
    end function store_value

    function store_make(self, factory) result(res)
        class(Store), intent(in) :: self
        procedure(imake) :: factory
        class(*), allocatable :: res
        res = factory(self)
    end function store_make

    integer function store_transformed(self, f)
        class(Store), intent(in) :: self
        procedure(itransform), optional :: f
        if (present(f)) then
            store_transformed = f(self%n)
        else
            store_transformed = self%n
        end if
    end function store_transformed

    subroutine store_visit(self, v, total)
        class(Store), intent(in) :: self
        procedure(ivisit) :: v
        integer, intent(inout) :: total
        call v(self, total)
        call v(self, total)
    end subroutine store_visit

    subroutine store_choose(self, f)
        class(Store), intent(in) :: self
        procedure(itransform), pointer, intent(out) :: f
        if (mod(self%n, 2) == 0) then
            f => twice
        else
            f => negate
        end if
    end subroutine store_choose

    subroutine store_swap(self, f)
        class(Store), intent(inout) :: self
        procedure(itransform), pointer, intent(inout) :: f
        procedure(itransform), pointer :: old
        old => self%saved
        self%saved => f
        f => old
    end subroutine store_swap

    integer function twice(n)
        integer, intent(in) :: n
        twice = 2 * n
    end function twice

    integer function negate(n)
        integer, intent(in) :: n
        negate = -n
    end function negate

    function make_pair(s) result(res)
        class(ISource), intent(in) :: s
        class(*), allocatable :: res
        res = Pair(s%value(), -s%value())
    end function make_pair

    function make_real(s) result(res)
        class(ISource), intent(in) :: s
        class(*), allocatable :: res
        res = 0.5d0 * s%value()
    end function make_real

    subroutine add_value(s, total)
        class(ISource), intent(in) :: s
        integer, intent(inout) :: total
        total = total + s%value()
    end subroutine add_value

    subroutine check(s, n)
        class(IStore), intent(inout) :: s
        integer, intent(in) :: n
        class(*), allocatable :: r
        procedure(itransform), pointer :: f
        integer :: total
        r = s%make(make_pair)
        select type (r)
        type is (Pair)
            if (r%a /= n .or. r%b /= -n) error stop 1
        class default
            error stop 2
        end select
        r = s%make(make_real)
        select type (r)
        type is (real(8))
            if (abs(r - 0.5d0 * n) > 1d-12) error stop 3
        class default
            error stop 4
        end select
        if (s%transformed() /= n) error stop 5
        if (s%transformed(twice) /= 2 * n) error stop 6
        if (s%transformed(f=negate) /= -n) error stop 7
        total = 1
        call s%visit(add_value, total)
        if (total /= 1 + 2 * n) error stop 8
        f => null()
        call s%choose(f)
        if (.not. associated(f)) error stop 9
        if (mod(n, 2) == 0) then
            if (f(5) /= 10) error stop 10
        else
            if (f(5) /= -5) error stop 11
        end if
        f => twice
        call s%swap(f)
        if (associated(f)) error stop 12
        f => negate
        call s%swap(f)
        if (.not. associated(f, twice)) error stop 13
        if (f(3) /= 6) error stop 14
        f => null()
        call s%swap(f)
        if (.not. associated(f, negate)) error stop 15
    end subroutine check
end module traits_runtime_procedure_argument_02_m

program traits_runtime_procedure_argument_02
    use traits_runtime_procedure_argument_02_m
    implicit none
    class(IStore), allocatable :: owner
    class(IStore), pointer :: p
    type(Store), target :: s
    class(*), allocatable :: r
    procedure(itransform), pointer :: f

    s%n = 4
    call check(s, 4)
    allocate(owner, source=Store(7))
    call check(owner, 7)
    p => s
    call check(p, 4)
    r = owner%make(make_pair)
    select type (r)
    type is (Pair)
        if (r%a /= 7) error stop 20
    class default
        error stop 21
    end select
    if (owner%transformed(negate) /= -7) error stop 22
    call p%choose(f)
    if (f(1) /= 2) error stop 23
    print *, "ok"
end program traits_runtime_procedure_argument_02
