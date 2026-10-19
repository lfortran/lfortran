! The standard Fortran counterpart of traits_runtime_constructor_component_02,
! with one abstract type in place of the traits and their combination.
! GFortran 16 shares rather than copies a polymorphic data source of an
! allocatable component in a structure constructor, and fails to replace an
! allocated component of an assignment target by the value of a constructor,
! so the data sources here are nonpolymorphic or unallocated, each value gets
! a fresh target, and finalization counts are not checked. It also fails to
! compile constructors of an extension of such a type, which are left out.
module traits_runtime_constructor_component_02_oracle_m
    implicit none
    type, abstract :: IAB
    contains
        procedure(geta_i), deferred :: geta
        procedure(getb_i), deferred :: getb
    end type
    abstract interface
        integer function geta_i(self)
            import IAB
            class(IAB), intent(in) :: self
        end function
        integer function getb_i(self)
            import IAB
            class(IAB), intent(in) :: self
        end function
    end interface
    type, extends(IAB) :: Base
        integer :: n = 1
    contains
        procedure :: geta => base_geta
        procedure :: getb => base_getb
    end type
    type, extends(Base) :: Ext
    contains
        procedure :: geta => ext_geta
    end type
    type :: Pair
        integer :: before = -1
        class(IAB), allocatable :: a
        class(IAB), allocatable :: ab
        character(len=:), allocatable :: label
        class(IAB), allocatable :: b
    end type
    type :: Outer
        type(Pair) :: p
        class(IAB), allocatable :: extra
    end type
contains
    integer function base_geta(self)
        class(Base), intent(in) :: self
        base_geta = self%n
    end function
    integer function ext_geta(self)
        class(Ext), intent(in) :: self
        ext_geta = 100 + self%n
    end function
    integer function base_getb(self)
        class(Base), intent(in) :: self
        base_getb = 10 * self%n
    end function
    integer function total(p)
        type(Pair), intent(in) :: p
        total = 0
        if (allocated(p%a)) total = total + p%a%geta()
        if (allocated(p%ab)) total = total + p%ab%geta() + p%ab%getb()
        if (allocated(p%b)) total = total + p%b%getb()
    end function
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

program traits_runtime_constructor_component_02_oracle
    use traits_runtime_constructor_component_02_oracle_m
    implicit none
    type(Pair) :: x1, x2, x3
    type(Pair), allocatable :: arr(:)
    type(Outer) :: o
    class(*), allocatable :: res
    class(IAB), allocatable :: none
    type(Base) :: t
    type(Ext) :: e

    t%n = 4
    e%n = 5

    ! Ordinary and trait components, positional and by keyword.
    x1 = Pair(7, Base(2), t, "hi", t)
    call check(x1%before == 7 .and. x1%a%geta() == 2 .and. x1%ab%getb() == 40, 1)
    call check(x1%label == "hi" .and. x1%b%getb() == 40, 2)
    t%n = 9
    call check(x1%ab%getb() == 40 .and. x1%b%getb() == 40, 3)
    x2 = Pair(a=e, ab=Base(6), b=t)
    call check(x2%a%geta() == 105 .and. x2%ab%geta() == 6 .and. x2%b%getb() == 90, 5)
    call check(.not. allocated(x2%label) .and. x2%before == -1, 6)

    ! An unallocated source leaves the component unallocated.
    x3 = Pair(a=t, ab=none, b=none)
    call check(x3%a%geta() == 9, 7)
    call check(.not. allocated(x3%ab) .and. .not. allocated(x3%b), 8)

    ! Nested constructors, unlimited polymorphic results, array elements.
    o = Outer(Pair(a=Base(2), ab=t), e)
    call check(o%p%a%geta() == 2 .and. o%p%ab%geta() == 9 .and. o%extra%geta() == 105, 12)
    res = Pair(a=t, ab=t)
    select type (res)
    type is (Pair)
        call check(res%a%geta() == 9 .and. res%ab%getb() == 90, 13)
    class default
        error stop 14
    end select
    allocate(arr(2))
    arr = [Pair(a=Base(2), ab=t), Pair(a=e, ab=Base(5))]
    call check(arr(1)%a%geta() == 2 .and. arr(2)%a%geta() == 105, 15)
    call check(arr(2)%ab%getb() == 50, 16)
    call check(total(Pair(a=t, ab=Base(1), b=e)) == 9 + 1 + 10 + 50, 17)
    print '(a)', 'traits_runtime_constructor_component_02_oracle ok'
end program
