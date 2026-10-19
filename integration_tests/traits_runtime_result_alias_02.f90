! An allocatable result of a message is assigned to a variable that the
! implementation reads: a module variable, a component of one, a dummy
! argument associated with one, a TARGET variable a module pointer is
! associated with, and a host variable. The implementation sees the
! variable's value from before the assignment; only a variable no callee
! can see receives the result directly.
module traits_runtime_result_alias_02_m
    implicit none
    type :: Box
        real(8) :: x = 0
    end type
    abstract interface :: IGet
        function get(self) result(r)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: r
        end function
        function tag(self) result(r)
            type(itself), pass, intent(in) :: self
            character(len=:), allocatable :: r
        end function
        function make_box(self) result(r)
            import :: Box
            type(itself), pass, intent(in) :: self
            type(Box), allocatable :: r
        end function
        function grow(self) result(r)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: r(:)
        end function
    end interface
    type :: Holder
        real(8), allocatable :: v
    end type
    type :: G
        real(8) :: k = 1
    end type
    implements IGet :: G
        procedure, pass :: get => g_get
        procedure, pass :: tag => g_tag
        procedure, pass :: make_box => g_box
        procedure, pass :: grow => g_grow
    end implements
    real(8), allocatable :: gv
    character(len=:), allocatable :: gs
    type(Box), allocatable :: gb
    real(8), allocatable :: gy(:)
    type(Holder) :: gh
    real(8), pointer :: gp => null()
contains
    function g_get(self) result(r)
        class(G), intent(in) :: self
        real(8), allocatable :: r
        if (associated(gp)) then
            r = gp + self%k
        else if (allocated(gh%v)) then
            r = gh%v + self%k
        else
            r = gv + self%k
        end if
    end function
    function g_tag(self) result(r)
        class(G), intent(in) :: self
        character(len=:), allocatable :: r
        r = '<' // gs // '>'
    end function
    function g_box(self) result(r)
        class(G), intent(in) :: self
        type(Box), allocatable :: r
        allocate(r)
        r%x = gb%x + self%k
    end function
    function g_grow(self) result(r)
        class(G), intent(in) :: self
        real(8), allocatable :: r(:)
        allocate(r(size(gy) + 1))
        r(1:size(gy)) = gy
        r(size(r)) = self%k
    end function

    ! The dummies are associated with the module variables.
    subroutine through_dummies(a, v, s)
        class(IGet), intent(in) :: a
        real(8), allocatable, intent(inout) :: v
        character(len=:), allocatable, intent(inout) :: s
        v = a%get()
        s = a%tag()
    end subroutine
end module

program traits_runtime_result_alias_02
    use traits_runtime_result_alias_02_m
    implicit none
    class(IGet), allocatable :: a
    real(8), allocatable :: lv
    real(8), allocatable, target :: tv
    character(len=:), allocatable :: ls

    allocate(a, source=G())

    gv = 10
    gv = a%get()
    print *, gv
    if (gv /= 11) error stop 1

    gs = 'g'
    gs = a%tag()
    print *, gs
    if (gs /= '<g>') error stop 2

    allocate(gb)
    gb%x = 3
    gb = a%make_box()
    print *, gb%x
    if (gb%x /= 4) error stop 3

    allocate(gy(2))
    gy = [1d0, 2d0]
    gy = a%grow()
    print *, gy
    if (size(gy) /= 3) error stop 4
    if (any(gy /= [1d0, 2d0, 1d0])) error stop 5

    gh%v = 20
    gh%v = a%get()
    print *, gh%v
    if (gh%v /= 21) error stop 6
    deallocate(gh%v)

    call through_dummies(a, gv, gs)
    print *, gv, gs
    if (gv /= 12) error stop 7
    if (gs /= '<<g>>') error stop 8

    tv = 30
    gp => tv
    tv = a%get()
    print *, tv
    if (tv /= 31) error stop 9
    nullify(gp)

    ! A local variable no callee can see.
    ls = 'l'
    ls = a%tag()
    print *, ls
    if (ls /= '<<<g>>>') error stop 10

    lv = 5
    call inner()
    print *, lv
    if (lv /= 15) error stop 11
contains
    subroutine inner()
        lv = a%get() + 1
        gv = lv
        lv = a%get()
    end subroutine
end program
