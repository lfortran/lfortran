! Standard-Fortran oracle of traits_host_association_01: the same program with
! an ordinary abstract base type in place of the trait.
module traits_host_association_01_oracle_m
    implicit none
    type, abstract :: IVal
    contains
        procedure(get_iface), deferred :: get
        procedure(add_iface), deferred :: add
    end type
    abstract interface
        integer function get_iface(self)
            import IVal
            class(IVal), intent(in) :: self
        end function
        subroutine add_iface(self, n)
            import IVal
            class(IVal), intent(inout) :: self
            integer, intent(in) :: n
        end subroutine
    end interface
    type, extends(IVal) :: Cell
        integer :: n = 0
    contains
        procedure :: get => cell_get
        procedure :: add => cell_add
        final :: finish
    end type
    type, extends(IVal) :: Other
        integer :: n = 0
    contains
        procedure :: get => other_get
        procedure :: add => other_add
    end type
    integer :: finals = 0
contains
    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%n
    end function
    subroutine cell_add(self, n)
        class(Cell), intent(inout) :: self
        integer, intent(in) :: n
        self%n = self%n + n
    end subroutine
    integer function other_get(self)
        class(Other), intent(in) :: self
        other_get = 100 + self%n
    end function
    subroutine other_add(self, n)
        class(Other), intent(inout) :: self
        integer, intent(in) :: n
        self%n = self%n + 2 * n
    end subroutine
    subroutine finish(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
    end subroutine

    ! A borrowed view dummy used by internal procedures.
    integer function solve(v) result(r)
        class(IVal), intent(inout) :: v
        call twice()
        if (v%get() /= peek() .or. peek() /= look(v)) error stop 1
        call v%add(1)
        r = peek()
    contains
        subroutine twice()
            call v%add(1)
            call v%add(1)
        end subroutine
        integer function peek()
            peek = v%get()
        end function
        integer function look(w)
            class(IVal), intent(in) :: w
            look = w%get()
            if (look /= v%get()) error stop 2
        end function
    end function

    ! A readonly view dummy inspected by an internal procedure.
    integer function inspect(v) result(r)
        class(IVal), intent(in) :: v
        r = 0
        call find()
    contains
        subroutine find()
            select type (c => v)
            type is (Cell)
                r = c%n
            type is (Other)
                r = -c%n
            end select
        end subroutine
    end function

    ! A readonly allocatable owner dummy lent to internal procedures.
    integer function lent(o) result(r)
        class(IVal), allocatable, intent(in) :: o
        r = -1
        call look()
        if (allocated(o)) then
            if (r /= o%get() .or. r /= through_view()) error stop 4
        else if (has_value()) then
            error stop 5
        end if
    contains
        subroutine look()
            if (allocated(o)) r = o%get()
        end subroutine
        logical function has_value()
            has_value = allocated(o)
        end function
        integer function through_view()
            through_view = borrow(o)
        end function
    end function

    integer function borrow(w)
        class(IVal), intent(in) :: w
        borrow = w%get()
    end function

    ! A local owner of a procedure, released when the procedure returns.
    integer function scoped() result(r)
        class(IVal), allocatable :: t
        call build()
        call grow(5)
        r = t%get()
    contains
        subroutine build()
            if (allocated(t)) error stop 3
            allocate(Cell :: t)
        end subroutine
        subroutine grow(k)
            integer, intent(in) :: k
            call t%add(k)
        end subroutine
    end function
end module

program traits_host_association_01_oracle
    use traits_host_association_01_oracle_m
    implicit none
    class(IVal), allocatable, target :: o
    class(IVal), pointer :: q => null()
    type(Cell), target :: c
    type(Other), target :: d
    type(Cell) :: e
    integer :: f0

    ! Allocation by an internal procedure defines the host owner.
    if (lent(o) /= -1) error stop 32
    call make_it()
    if (.not. allocated(o)) error stop 10
    if (o%get() /= 3 .or. lent(o) /= 3) error stop 11

    ! Mutation through host association keeps the same payload: no copies
    ! are made or finalized, and a pointer to the owner sees each update.
    q => o
    f0 = finals
    call bump()
    call bump()
    if (o%get() /= 5 .or. q%get() /= 5) error stop 12
    if (lent(o) /= 5 .or. q%get() /= 5) error stop 33
    if (finals /= f0) error stop 13
    if (peek() /= 5 .or. o%get() + peek() /= 10) error stop 14
    call o%add(1)
    if (peek() /= 6) error stop 15
    do while (peek() < 9 .and. o%get() > 0)
        call o%add(1)
    end do
    if (o%get() /= 9 .or. q%get() /= 9) error stop 16
    do while (peek() < 100)
        if (o%get() >= 10) exit
        call o%add(1)
    end do
    if (o%get() /= 10 .or. q%get() /= 10) error stop 17
    if (finals /= f0) error stop 18
    if (kind_of() /= 10) error stop 31

    ! Deallocation by an internal procedure finalizes the payload once.
    nullify(q)
    call drop()
    if (allocated(o)) error stop 19
    if (lent(o) /= -1) error stop 34
    if (finals /= f0 + 1) error stop 20

    ! A host pointer: messages, mutation and reassociation.
    c%n = 1
    q => c
    call push()
    if (c%n /= 3 .or. q%get() /= 3) error stop 21
    d%n = 4
    call retarget()
    if (.not. associated(q)) error stop 22
    if (q%get() /= 104) error stop 23
    call push()
    if (d%n /= 8 .or. q%get() /= 108) error stop 24

    ! Borrowed views reached from internal procedures of a module procedure.
    e%n = 3
    if (solve(e) /= 6 .or. e%n /= 6) error stop 25
    if (solve(d) /= 114 .or. d%n /= 14) error stop 26
    if (inspect(e) /= 6 .or. inspect(d) /= -14) error stop 27

    ! A procedure-local owner is finalized when the procedure returns.
    f0 = finals
    if (scoped() /= 5) error stop 28
    if (finals /= f0 + 1) error stop 29
    print *, "PASS"
contains
    subroutine make_it()
        if (allocated(o)) error stop 30
        o = Cell(3)
    end subroutine
    subroutine bump()
        call o%add(1)
    end subroutine
    integer function peek()
        peek = o%get()
    end function
    integer function kind_of()
        kind_of = 0
        select type (x => o)
        type is (Cell)
            kind_of = x%n
        end select
    end function
    subroutine drop()
        deallocate(o)
    end subroutine
    subroutine push()
        call q%add(1)
        call q%add(1)
    end subroutine
    subroutine retarget()
        q => d
    end subroutine
end program
