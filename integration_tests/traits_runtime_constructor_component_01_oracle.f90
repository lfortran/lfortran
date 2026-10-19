! The standard Fortran counterpart of traits_runtime_constructor_component_01,
! with an abstract type in place of the trait. GFortran 16 shares rather than
! copies a polymorphic data source of an allocatable component in a structure
! constructor, so the default constructor gets only nonpolymorphic and
! unallocated sources here, and polymorphic ones go through the equivalent
! user-defined constructor. It also fails to replace an allocated component
! of an assignment target by the value of a constructor, so each value gets a
! fresh target, and the payload has no allocatable components of its own,
! which GFortran 16 does not copy either; finalization counts are not checked.
module traits_runtime_constructor_component_01_oracle_m
    implicit none
    integer :: finals = 0
    type, abstract :: ICount
    contains
        procedure(get_i), deferred :: get
        procedure(bump_i), deferred :: bump
    end type
    abstract interface
        integer function get_i(self)
            import ICount
            class(ICount), intent(in) :: self
        end function
        subroutine bump_i(self)
            import ICount
            class(ICount), intent(inout) :: self
        end subroutine
    end interface
    type, extends(ICount) :: Counter
        integer :: n = 0
    contains
        procedure :: get => counter_get
        procedure :: bump => counter_bump
        final :: finish
    end type
    type, extends(Counter) :: Doubler
    contains
        procedure :: get => doubler_get
    end type
    type :: Holder
        class(ICount), allocatable :: tobj
    end type
    type :: Outer
        type(Holder) :: inner
        integer :: tag = 0
    end type
    ! The user-defined constructor that the default one replaces.
    type :: UHolder
        class(ICount), allocatable :: tobj
    end type
    interface UHolder
        module procedure make_uholder
    end interface
contains
    integer function counter_get(self)
        class(Counter), intent(in) :: self
        counter_get = self%n
    end function
    subroutine counter_bump(self)
        class(Counter), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    integer function doubler_get(self)
        class(Doubler), intent(in) :: self
        doubler_get = 2 * counter_get(self)
    end function
    subroutine finish(self)
        type(Counter), intent(inout) :: self
        finals = finals + 1
    end subroutine
    function make_uholder(obj) result(self)
        class(ICount), intent(in) :: obj
        type(UHolder) :: self
        self%tobj = obj
    end function
    function from_view(view) result(h)
        class(ICount), intent(in) :: view
        type(Holder) :: h
        h = Holder(view)
    end function
    function from_slot(obj) result(h)
        class(ICount), allocatable, intent(in) :: obj
        type(Holder) :: h
        h = Holder(tobj=obj)
    end function
    function make_counter(n) result(obj)
        integer, intent(in) :: n
        class(ICount), allocatable :: obj
        type(Counter) :: c
        c%n = n
        allocate(obj, source=c)
    end function
    ! The function of the original report: an unlimited polymorphic result.
    function as_any(obj) result(res)
        class(ICount), allocatable, intent(in) :: obj
        class(*), allocatable :: res
        res = Holder(obj)
    end function
    integer function peek(h)
        type(Holder), intent(in) :: h
        peek = -1
        if (allocated(h%tobj)) peek = h%tobj%get()
    end function
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

program traits_runtime_constructor_component_01_oracle
    use traits_runtime_constructor_component_01_oracle_m
    implicit none
    type(Holder) :: x1, x2, x3, x4, x5, x6, x7, xs(3)
    type(Holder), allocatable :: z
    type(UHolder) :: u1, u2, u3, u4, u5
    type(Outer) :: o
    type(Counter) :: c
    type(Counter), target :: t
    class(Counter), allocatable :: poly
    class(ICount), allocatable :: obj, none
    class(ICount), pointer :: p
    class(*), allocatable :: res

    c%n = 1
    obj = c

    ! The user-defined constructor copies an allocated owner.
    u1 = UHolder(obj)
    call check(u1%tobj%get() == 1, 1)
    call u1%tobj%bump()
    call check(u1%tobj%get() == 2 .and. obj%get() == 1, 2)

    ! An unallocated source leaves the component unallocated, as do null()
    ! and omitting it.
    x1 = Holder(none)
    call check(.not. allocated(x1%tobj), 6)
    x2 = Holder(null())
    call check(.not. allocated(x2%tobj), 7)
    x3 = Holder()
    call check(.not. allocated(x3%tobj), 8)

    ! Concrete values, variables and constructors alike.
    x4 = Holder(c)
    u2 = UHolder(c)
    call check(x4%tobj%get() == 1 .and. u2%tobj%get() == 1, 9)
    c%n = 2
    call check(x4%tobj%get() == 1 .and. u2%tobj%get() == 1, 10)
    x5 = Holder(Counter(5))
    call check(x5%tobj%get() == 5, 11)

    ! Owners, pointers and class(T) objects through the user-defined
    ! constructor.
    call obj%bump()
    call obj%bump()
    u3 = UHolder(obj)
    call check(u3%tobj%get() == 3, 12)
    t%n = 7
    p => t
    u4 = UHolder(p)
    t%n = 8
    call check(u4%tobj%get() == 7 .and. p%get() == 8, 15)
    allocate(Doubler :: poly)
    poly%n = 3
    u5 = UHolder(poly)
    poly%n = 4
    call check(u5%tobj%get() == 6, 16)

    ! Usage contexts: unlimited polymorphic results, SOURCE=, actual
    ! arguments, array constructors, nested constructors and associations.
    res = Holder(c)
    select type (res)
    type is (Holder)
        call check(res%tobj%get() == 2, 23)
    class default
        error stop 24
    end select
    deallocate(res)
    res = as_any(none)
    select type (res)
    type is (Holder)
        call check(.not. allocated(res%tobj), 25)
    class default
        error stop 26
    end select
    deallocate(res)
    allocate(res, source=Holder(Counter(6)))
    select type (res)
    type is (Holder)
        call check(res%tobj%get() == 6, 27)
    class default
        error stop 28
    end select
    allocate(z, source=Holder(c))
    call check(z%tobj%get() == 2, 29)
    call check(peek(Holder(c)) == 2 .and. peek(Holder(none)) == -1, 30)
    call check(peek(Holder(Counter(4))) == 4, 31)
    xs = [Holder :: Holder(Counter(3)), Holder(c), Holder(none)]
    call check(xs(1)%tobj%get() == 3 .and. xs(2)%tobj%get() == 2, 32)
    call check(.not. allocated(xs(3)%tobj), 33)
    o = Outer(Holder(c), 6)
    call check(o%inner%tobj%get() == 2 .and. o%tag == 6, 34)
    associate (h => Holder(c))
        call check(h%tobj%get() == 2, 35)
    end associate
    x6 = Holder(z%tobj)
    x7 = x6
    call check(x7%tobj%get() == 2, 36)
    deallocate(z)
    print '(a)', 'traits_runtime_constructor_component_01_oracle ok'
end program
