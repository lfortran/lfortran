! A scalar class pointer passed to a polymorphic dummy without POINTER or
! ALLOCATABLE: the dummy is associated with the target, so re-associating,
! reallocating or nullifying the pointer during the call leaves it alone.
module derived_types_224_fsm
implicit none
type, abstract :: state
    integer :: n = 0
contains
    procedure(step_i), deferred :: step
end type
type :: machine
    class(state), pointer :: st => null()
end type
abstract interface
    subroutine step_i(this, m)
        import :: state, machine
        class(state), intent(in) :: this
        type(machine), intent(inout) :: m
    end subroutine
end interface
type, extends(state) :: red
contains
    procedure :: step => red_step
end type
type, extends(state) :: green
contains
    procedure :: step => green_step
end type
contains
subroutine red_step(this, m)
    class(red), intent(in) :: this
    type(machine), intent(inout) :: m
    allocate(green :: m%st)
    m%st%n = this%n + 1
end subroutine
subroutine green_step(this, m)
    class(green), intent(in) :: this
    type(machine), intent(inout) :: m
    allocate(red :: m%st)
    m%st%n = this%n + 1
end subroutine
end module

module derived_types_224_m
implicit none
type :: s
    integer :: x = 0
contains
    procedure :: meth
    procedure :: get
end type
type, extends(s) :: s2
    integer :: y = 5
end type
type :: t
    class(s), pointer :: p => null()
end type
type(t) :: g
type(s2), target, save :: z
class(s), pointer :: gp
class(*), pointer :: gu
contains
subroutine meth(self)
    class(s), intent(in) :: self
    g%p => null()
    if (self%x /= 7) error stop 1
end subroutine
integer function get(self)
    class(s), intent(in) :: self
    allocate(g%p)
    g%p%x = 99
    deallocate(g%p)
    get = self%x
end function
subroutine check_s7(c)
    class(s), intent(in) :: c
    select type (c)
    type is (s)
        if (c%x /= 7) error stop 2
    class default
        error stop 3
    end select
end subroutine
subroutine work(v, c, mode)
    type(t), intent(inout) :: v
    class(s), intent(in) :: c
    integer, intent(in) :: mode
    type(t) :: e
    select case (mode)
    case (1)
        v%p => null()
    case (2)
        allocate(v%p)
        v%p%x = 99
    case (3)
        nullify(v%p)
    case (4)
        v = e
    case (5)
        v%p => z
    case (6)
        e%p => z
        v = e
    end select
    call check_s7(c)
    if (mode == 2) deallocate(v%p)
end subroutine
subroutine work_local(mode)
    integer, intent(in) :: mode
    type(s), target :: y
    class(s), pointer :: lp
    y%x = 7
    lp => y
    call inner(lp)
contains
    subroutine inner(c)
        class(s), intent(in) :: c
        if (mode == 1) then
            lp => null()
        else
            allocate(lp)
            lp%x = 99
        end if
        call check_s7(c)
        if (mode == 2) deallocate(lp)
    end subroutine
end subroutine
subroutine work_upoly(c)
    class(*), intent(in) :: c
    gu => null()
    select type (c)
    type is (s)
        if (c%x /= 7) error stop 4
    class default
        error stop 5
    end select
end subroutine
subroutine set_gu(c)
    class(*), intent(in), target :: c
    gu => c
end subroutine
subroutine keep(c)
    class(s), intent(in), target :: c
    gp => c
end subroutine
subroutine opt(c, expect)
    class(s), intent(in), optional :: c
    logical, intent(in) :: expect
    if (present(c) .neqv. expect) error stop 6
    if (present(c)) call check_s7(c)
end subroutine
end module

program derived_types_224
use derived_types_224_fsm
use derived_types_224_m
implicit none
type(machine) :: mc
class(state), pointer :: old
type(s), target :: y
type(t) :: b
integer :: k, mode

allocate(red :: mc%st)
do k = 1, 6
    old => mc%st
    call mc%st%step(mc)
    deallocate(old)
end do
if (mc%st%n /= 6) error stop 10
select type (q => mc%st)
type is (red)
class default
    error stop 11
end select
deallocate(mc%st)

y%x = 7
z%x = 8
do k = 1, 3
    do mode = 1, 6
        b%p => y
        call work(b, b%p, mode)
    end do
    do mode = 1, 2
        call work_local(mode)
    end do

    g%p => y
    call g%p%meth()
    if (associated(g%p)) error stop 12
    g%p => y
    if (g%p%get() /= 7) error stop 13
    if (associated(g%p)) error stop 14

    call set_gu(y)
    call work_upoly(gu)
    if (associated(gu)) error stop 15

    b%p => y
    call keep(b%p)
    b%p => null()
    if (gp%x /= 7) error stop 16

    call opt(b%p, .false.)
    b%p => y
    call opt(b%p, .true.)
end do
print *, "ok"
end program
