! LFortran traits extension test; GFortran does not accept this syntax.
! Dummy procedures whose interfaces take trait views: an interface body of a
! module procedure and of an internal procedure, a dummy declared with
! procedure(iface) and a procedure pointer, each called with a trait object
! and with concrete implementers. The trait contract of a nested interface
! must be visible from the procedure that owns the dummy, also after the
! module is read back from its module file.
! traits_dummy_procedure_01_oracle.f90 is the standard-Fortran counterpart.
module traits_dummy_procedure_01_m
    implicit none
    abstract interface :: IVal
        integer function get(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine add(self, n)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: n
        end subroutine
    end interface
    type, implements(IVal) :: Cell
        integer :: n = 0
    contains
        procedure :: get => cell_get
        procedure :: add => cell_add
    end type
    type, implements(IVal) :: Pair
        integer :: a = 0, b = 0
    contains
        procedure :: get => pair_get
        procedure :: add => pair_add
    end type
    abstract interface
        subroutine update_iface(v)
            import IVal
            class(IVal), intent(inout) :: v
        end subroutine
    end interface
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
    integer function pair_get(self)
        class(Pair), intent(in) :: self
        pair_get = self%a + self%b
    end function
    subroutine pair_add(self, n)
        class(Pair), intent(inout) :: self
        integer, intent(in) :: n
        self%a = self%a + n
    end subroutine
end module

module traits_dummy_procedure_01_u
    use traits_dummy_procedure_01_m
    implicit none
contains
    subroutine inc(v)
        class(IVal), intent(inout) :: v
        call v%add(1)
    end subroutine
    subroutine dbl(v)
        class(IVal), intent(inout) :: v
        call v%add(v%get())
    end subroutine
    integer function triple(v)
        class(IVal), intent(in) :: v
        triple = 3 * v%get()
    end function
    subroutine apply(fn, v)
        interface
            subroutine fn(v)
                import IVal
                class(IVal), intent(inout) :: v
            end subroutine
        end interface
        class(IVal), intent(inout) :: v
        call fn(v)
    end subroutine
    subroutine apply_cell(fn, c)
        interface
            subroutine fn(v)
                import IVal
                class(IVal), intent(inout) :: v
            end subroutine
        end interface
        type(Cell), intent(inout) :: c
        call fn(c)
        call fn(c)
    end subroutine
    integer function query(f, p)
        interface
            integer function f(v)
                import IVal
                class(IVal), intent(in) :: v
            end function
        end interface
        type(Pair), intent(in) :: p
        query = f(p)
    end function
    subroutine apply_declared(fn, v)
        procedure(update_iface) :: fn
        class(IVal), intent(inout) :: v
        call fn(v)
    end subroutine
    subroutine apply_pointer(c, p)
        type(Cell), intent(inout) :: c
        type(Pair), intent(inout) :: p
        procedure(update_iface), pointer :: q
        q => inc
        call q(c)
        q => dbl
        call q(p)
        call q(c)
    end subroutine
end module

program traits_dummy_procedure_01
    use traits_dummy_procedure_01_m
    use traits_dummy_procedure_01_u
    implicit none
    type(Cell) :: c
    type(Pair) :: p
    class(IVal), allocatable :: obj

    call apply_cell(inc, c)
    if (c%n /= 2) error stop 1
    call apply(dbl, c)
    if (c%n /= 4) error stop 2
    p = Pair(2, 5)
    call apply(inc, p)
    if (p%a /= 3 .or. p%b /= 5) error stop 3
    if (query(triple, p) /= 24) error stop 4

    allocate(Cell :: obj)
    call apply(inc, obj)
    call apply(dbl, obj)
    call apply_declared(inc, obj)
    if (obj%get() /= 3) error stop 5
    deallocate(obj)
    allocate(Pair :: obj)
    call obj%add(4)
    call apply_declared(dbl, obj)
    if (obj%get() /= 8) error stop 6
    call apply_local(inc, obj)
    if (obj%get() /= 9) error stop 7
    deallocate(obj)

    call apply_declared(inc, c)
    if (c%n /= 5) error stop 8
    call apply_pointer(c, p)
    if (c%n /= 12) error stop 9
    if (p%a /= 11 .or. p%b /= 5) error stop 10
    call apply_local(dbl, p)
    if (p%a /= 27 .or. p%b /= 5) error stop 11
    print '(a)', 'traits_dummy_procedure_01 ok'
contains
    subroutine apply_local(fn, v)
        interface
            subroutine fn(v)
                import IVal
                class(IVal), intent(inout) :: v
            end subroutine
        end interface
        class(IVal), intent(inout) :: v
        call fn(v)
    end subroutine
end program
