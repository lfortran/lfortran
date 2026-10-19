! LFortran traits extension test; GFortran does not accept this syntax.
! Trait witnesses that are separate module procedures: their interfaces are
! in a module and their bodies in a submodule, either in other files
! (traits_submodule_binding_01_mod.f90 and traits_submodule_binding_01_impl.f90,
! which also has a client module) or in this one. Like ordinary type-bound
! procedures, the bindings name the interfaces. Calls dispatch through
! class(<trait>) dummies and allocatable trait objects, and statically through
! direct calls and generic procedures. traits_submodule_binding_01_oracle.f90
! is the standard-Fortran counterpart.
module traits_submodule_binding_01_local
    implicit none
    abstract interface :: IVal
        integer function get(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type, implements(IVal) :: Cell
        integer :: n = 3
    contains
        procedure :: get => cell_get
    end type
    interface
        module integer function cell_get(self)
            class(Cell), intent(in) :: self
        end function
    end interface
contains
    integer function get_value(x)
        class(IVal), intent(in) :: x
        get_value = x%get()
    end function
end module

submodule (traits_submodule_binding_01_local) traits_submodule_binding_01_local_s
contains
    module procedure cell_get
        cell_get = 10*self%n
    end procedure
end submodule

program traits_submodule_binding_01
    use traits_submodule_binding_01_m
    use traits_submodule_binding_01_client
    use traits_submodule_binding_01_local
    implicit none
    type(Account) :: a
    type(Wallet) :: w
    type(Cell) :: c
    class(IAccount), allocatable :: owner

    ! Direct calls.
    if (a%balance() /= 7) error stop 1
    call a%deposit(2)
    if (a%balance() /= 9) error stop 2
    if (w%balance() /= 100) error stop 3
    call w%deposit(1)
    if (w%balance() /= 200) error stop 4
    if (c%get() /= 30) error stop 5
    ! Dispatch through trait objects.
    if (observe(a) /= 9 .or. observe(w) /= 200) error stop 6
    call credit(a, 1)
    call credit(w, 2)
    if (a%n /= 10 .or. w%cents /= 400) error stop 7
    if (get_value(c) /= 30) error stop 8
    if (total(a, w) /= 410) error stop 13
    allocate(Wallet :: owner)
    call owner%deposit(3)
    if (owner%balance() /= 400 .or. observe(owner) /= 400) error stop 9
    deallocate(owner)
    allocate(Account :: owner)
    call credit(owner, 5)
    if (owner%balance() /= 12) error stop 10
    deallocate(owner)
    ! Static calls through constrained generic procedures.
    call credit_generic(a, 10)
    if (read_generic(a) /= 20) error stop 11
    call credit_generic(w, 1)
    if (read_generic(w) /= 500) error stop 12
    print '(a)', 'traits_submodule_binding_01 ok'
end program
