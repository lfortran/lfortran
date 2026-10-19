! Default structure constructors give values to allocatable runtime trait
! components (F2018 7.5.10): the component becomes an independent copy of the
! dynamic type of its value, or is unallocated when the value is an
! unallocated owner, as with an equivalent user-defined constructor.
module traits_runtime_constructor_component_01_m
    implicit none
    integer :: finals = 0
    abstract interface :: ICount
        integer function get(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine bump(self)
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
    type :: Counter
        integer :: n = 0
        integer, allocatable :: hist(:)
    contains
        procedure :: get => counter_get
        procedure :: bump => counter_bump
        final :: finish
    end type
    type, extends(Counter) :: Doubler
    contains
        procedure :: get => doubler_get
    end type
    implements ICount :: Counter
        procedure :: get => counter_get
        procedure :: bump => counter_bump
    end implements
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
        if (allocated(self%hist)) counter_get = counter_get + sum(self%hist)
    end function
    subroutine counter_bump(self)
        class(Counter), intent(inout) :: self
        integer, allocatable :: t(:)
        self%n = self%n + 1
        if (.not. allocated(self%hist)) allocate(self%hist(0))
        allocate(t(size(self%hist) + 1))
        t(1:size(self%hist)) = self%hist
        t(size(t)) = 100
        call move_alloc(t, self%hist)
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

program traits_runtime_constructor_component_01
    use traits_runtime_constructor_component_01_m
    implicit none
    type(Holder) :: x, y, xs(3)
    type(Holder), allocatable :: z
    type(UHolder) :: u
    type(Outer) :: o
    type(Counter) :: c
    type(Counter), target :: t
    class(Counter), allocatable :: poly
    class(ICount), allocatable :: obj, none
    class(ICount), pointer :: p
    class(*), allocatable :: res

    c%n = 1
    allocate(c%hist(1))
    c%hist(1) = 10
    obj = c

    ! An allocated owner is copied, and the copy is independent.
    x = Holder(obj)
    u = UHolder(obj)
    call check(x%tobj%get() == 11 .and. u%tobj%get() == 11, 1)
    call check(finals == 1, 101)
    call x%tobj%bump()
    call u%tobj%bump()
    call check(x%tobj%get() == 112 .and. u%tobj%get() == 112, 2)
    call check(finals == 1, 102)
    call check(obj%get() == 11, 3)
    call obj%bump()
    call obj%bump()
    call check(obj%get() == 213 .and. x%tobj%get() == 112, 4)
    call check(finals == 1, 104)

    ! An unallocated owner leaves the component unallocated, as does null()
    ! or omitting it; the old value of the target is finalized.
    x = Holder(none)
    call check(.not. allocated(x%tobj), 6)
    call check(finals == 2, 106)
    x = Holder(obj)
    x = Holder(null())
    call check(.not. allocated(x%tobj), 7)
    call check(finals == 3, 107)
    x = Holder(obj)
    x = Holder()
    call check(.not. allocated(x%tobj), 8)
    call check(finals == 4, 108)

    ! Concrete values, variables and constructors alike.
    x = Holder(c)
    u = UHolder(c)
    call check(x%tobj%get() == 11 .and. u%tobj%get() == 11, 9)
    call check(finals == 6, 109)
    c%n = 2
    call check(x%tobj%get() == 11, 10)
    call check(finals == 6, 110)
    x = Holder(Counter(5))
    call check(x%tobj%get() == 5, 11)
    call check(finals == 7, 111)

    ! A view, a borrowed allocation slot and a trait pointer.
    y = from_view(obj)
    call check(y%tobj%get() == 213, 12)
    call check(finals == 8, 112)
    y = from_slot(obj)
    call check(y%tobj%get() == 213, 13)
    call check(finals == 10, 113)
    y = from_slot(none)
    call check(.not. allocated(y%tobj), 14)
    call check(finals == 11, 114)
    t%n = 7
    p => t
    x = Holder(p)
    u = UHolder(p)
    t%n = 8
    call check(x%tobj%get() == 7 .and. u%tobj%get() == 7 .and. p%get() == 8, 15)
    call check(finals == 14, 115)

    ! A class(T) object keeps its dynamic type and its overrides.
    allocate(Doubler :: poly)
    poly%n = 3
    x = Holder(poly)
    u = UHolder(poly)
    call check(x%tobj%get() == 6 .and. u%tobj%get() == 6, 16)
    call check(finals == 17, 116)
    poly%n = 4
    call check(x%tobj%get() == 6, 17)
    call check(finals == 17, 117)

    ! An associate name of a trait guard, a function result and the trait
    ! component of another object.
    deallocate(poly)
    allocate(Counter :: poly)
    poly%n = 9
    select type (q => poly)
    class is (ICount)
        x = Holder(q)
    class default
        error stop 18
    end select
    call check(x%tobj%get() == 9, 19)
    call check(finals == 19, 119)
    x = Holder(make_counter(21))
    call check(x%tobj%get() == 21, 20)
    call check(finals == 22, 120)
    y = Holder(x%tobj)
    call bump_twice(x)
    call check(y%tobj%get() == 21 .and. x%tobj%get() == 223, 21)
    call check(finals == 22, 121)
    x = Holder(x%tobj)
    call check(x%tobj%get() == 223, 22)
    call check(finals == 23, 122)

    ! Usage contexts: unlimited polymorphic results, SOURCE=, actual
    ! arguments, array constructors, nested constructors and associations.
    res = as_any(obj)
    select type (res)
    type is (Holder)
        call check(res%tobj%get() == 213, 23)
        call check(finals == 24, 123)
    class default
        error stop 24
    end select
    res = as_any(none)
    select type (res)
    type is (Holder)
        call check(.not. allocated(res%tobj), 25)
        call check(finals == 25, 125)
    class default
        error stop 26
    end select
    deallocate(res)
    allocate(res, source=Holder(obj))
    select type (res)
    type is (Holder)
        call check(res%tobj%get() == 213, 27)
        call check(finals == 25, 127)
    class default
        error stop 28
    end select
    allocate(z, source=Holder(c))
    call check(z%tobj%get() == 12, 29)
    call check(finals == 25, 129)
    call check(peek(Holder(obj)) == 213 .and. peek(Holder(none)) == -1, 30)
    call check(finals == 25, 130)
    call check(peek(Holder(Counter(4))) == 4, 31)
    call check(finals == 25, 131)
    xs = [Holder :: Holder(obj), Holder(c), Holder(none)]
    call check(xs(1)%tobj%get() == 213 .and. xs(2)%tobj%get() == 12, 32)
    call check(finals == 25, 132)
    call check(.not. allocated(xs(3)%tobj), 33)
    call check(finals == 25, 133)
    o = Outer(Holder(obj), 6)
    call check(o%inner%tobj%get() == 213 .and. o%tag == 6, 34)
    call check(finals == 25, 134)
    ! The associate name is a copy of the value until the construct ends.
    associate (h => Holder(c))
        call check(h%tobj%get() == 12, 35)
        call check(finals == 25, 135)
    end associate
    call check(finals == 26, 136)
    deallocate(z)
    call check(finals == 27, 137)
    print '(a)', 'traits_runtime_constructor_component_01 ok'
contains
    subroutine bump_twice(h)
        type(Holder), intent(inout) :: h
        call h%tobj%bump()
        call h%tobj%bump()
    end subroutine
end program
