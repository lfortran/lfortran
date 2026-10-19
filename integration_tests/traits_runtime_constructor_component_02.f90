! Default structure constructors of types with several runtime trait
! components, of combined traits, beside ordinary components and in type
! extensions: each trait component copies its value, projected to the
! component's trait, or stays unallocated for an unallocated owner.
module traits_runtime_constructor_component_02_m
    implicit none
    integer :: finals = 0
    abstract interface :: IA
        integer function geta(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: IB
        integer function getb(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine setb(self, n)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: n
        end subroutine
    end interface
    type :: Base
        integer :: n = 1
    contains
        procedure :: geta => base_geta
        procedure :: getb => base_getb
        procedure :: setb => base_setb
        final :: finish
    end type
    type, extends(Base) :: Ext
    contains
        procedure :: geta => ext_geta
    end type
    implements IA :: Base
        procedure :: geta => base_geta
    end implements
    implements IB :: Base
        procedure :: getb => base_getb
        procedure :: setb => base_setb
    end implements
    type :: Pair
        integer :: before = -1
        class(IA), allocatable :: a
        class(IA+IB), allocatable :: ab
        character(len=:), allocatable :: label
        class(IB), allocatable :: b
    end type
    type, extends(Pair) :: Triple
        class(IA), allocatable :: c
        integer :: after = -2
    end type
    type :: Outer
        type(Pair) :: p
        class(IA), allocatable :: extra
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
    subroutine base_setb(self, n)
        class(Base), intent(inout) :: self
        integer, intent(in) :: n
        self%n = n
    end subroutine
    subroutine finish(self)
        type(Base), intent(inout) :: self
        finals = finals + 1
    end subroutine
    integer function total(p)
        type(Pair), intent(in) :: p
        total = 0
        if (allocated(p%a)) total = total + p%a%geta()
        if (allocated(p%ab)) total = total + p%ab%geta() + p%ab%getb()
        if (allocated(p%b)) total = total + p%b%getb()
    end function
    ! Constructors of a view argument in a loop release every copy.
    subroutine repeat_views(view, n, sum_out)
        class(IA+IB), intent(in) :: view
        integer, intent(in) :: n
        integer, intent(out) :: sum_out
        type(Pair) :: p
        integer :: i
        sum_out = 0
        do i = 1, n
            p = Pair(i, view, view, "x", view)
            sum_out = sum_out + total(p) + total(Pair(ab=view)) + p%before
        end do
    end subroutine
    subroutine swap(p)
        type(Pair), intent(inout) :: p
        p = Pair(p%before, p%ab, p%ab, p%label, p%b)
    end subroutine
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

program traits_runtime_constructor_component_02
    use traits_runtime_constructor_component_02_m
    implicit none
    type(Pair) :: x
    type(Pair), allocatable :: arr(:)
    type(Triple) :: tr
    type(Outer) :: o
    class(*), allocatable :: res
    class(IA), allocatable :: a
    class(IA+IB), allocatable :: ab, abnone
    class(IA), pointer :: pa
    class(Base), allocatable :: poly
    type(Base), target :: t
    integer :: s

    t%n = 4
    a = Base(2)
    ab = Base(3)
    allocate(Ext :: poly)
    poly%n = 5

    ! Ordinary and trait components, positional and by keyword.
    x = Pair(7, a, ab, "hi", ab)
    call check(x%before == 7 .and. x%a%geta() == 2 .and. x%ab%getb() == 30, 1)
    call check(x%label == "hi" .and. x%b%getb() == 30, 2)
    call ab%setb(9)
    call check(x%ab%getb() == 30 .and. x%b%getb() == 30 .and. ab%getb() == 90, 3)
    call x%b%setb(4)
    call check(x%b%getb() == 40 .and. x%ab%getb() == 30, 4)
    call check(finals == 0, 104)
    x = Pair(a=poly, ab=Base(6), b=t)
    call check(x%a%geta() == 105 .and. x%ab%geta() == 6 .and. x%b%getb() == 40, 5)
    call check(.not. allocated(x%label) .and. x%before == -1, 6)
    call check(finals == 3, 106)

    ! A combined owner projected to a component of one of its traits; an
    ! unallocated one leaves both kinds of components unallocated.
    x = Pair(a=ab, ab=abnone, b=abnone)
    call check(x%a%geta() == 9, 7)
    call check(.not. allocated(x%ab) .and. .not. allocated(x%b), 8)
    call check(finals == 6, 108)

    ! A trait pointer and the parent components of an extension.
    pa => t
    tr = Triple(1, pa, ab, "t", ab, pa, 3)
    t%n = 8
    call check(tr%a%geta() == 4 .and. tr%c%geta() == 4 .and. pa%geta() == 8, 9)
    call check(tr%ab%getb() == 90 .and. tr%label == "t" .and. tr%after == 3, 10)
    tr = Triple(c=poly, b=ab)
    call check(.not. allocated(tr%a) .and. tr%c%geta() == 105 .and. tr%b%getb() == 90, 11)
    call check(finals == 10, 111)

    ! Nested constructors, unlimited polymorphic results, array elements.
    o = Outer(Pair(a=a, ab=ab), poly)
    call check(o%p%a%geta() == 2 .and. o%p%ab%geta() == 9 .and. o%extra%geta() == 105, 12)
    res = Pair(a=ab, ab=ab)
    select type (res)
    type is (Pair)
        call check(res%a%geta() == 9 .and. res%ab%getb() == 90, 13)
    class default
        error stop 14
    end select
    allocate(arr(2))
    arr = [Pair(a=a, ab=ab), Pair(a=poly, ab=Base(5))]
    call check(arr(1)%a%geta() == 2 .and. arr(2)%a%geta() == 105, 15)
    call check(arr(2)%ab%getb() == 50, 16)
    arr(1) = Pair(a=arr(2)%a, ab=arr(1)%ab, b=arr(2)%ab)
    call check(arr(1)%a%geta() == 105 .and. arr(1)%b%getb() == 50, 17)
    call check(finals == 12, 117)

    ! The value reads the target being assigned, so it is built in a
    ! temporary of swap, which keeps its copies until swap returns.
    x = Pair(2, a, ab, "s", t)
    call check(finals == 13, 118)
    call swap(x)
    call check(x%a%geta() == 9 .and. x%b%getb() == 80 .and. x%label == "s", 18)
    call check(finals == 13 + 3 + 3, 218)

    ! Copies made in a loop of a procedure are released.
    call repeat_views(ab, 3, s)
    call check(s == 3 * (9 + 9 + 90 + 90) + 3 * (9 + 90) + 6, 19)
    ! Each reassignment of p and of the temporary for Pair(ab=view) finalizes
    ! the copies it replaces; the rest are finalized when the procedure returns.
    call check(finals == 19 + 2 * 3 + 2 + 3 + 1, 119)
    print '(a)', 'traits_runtime_constructor_component_02 ok'
end program
