! Standard-Fortran counterpart of traits_submodule_binding_01.f90: abstract
! base types whose deferred bindings are implemented by separate module
! procedures, declared in modules and implemented in submodules. It makes the
! same direct, polymorphic and allocatable-object calls and checks the same
! values.
module traits_submodule_binding_01_oracle_m
    implicit none
    type, abstract :: IAccount
    contains
        procedure(balance_iface), deferred :: balance
        procedure(deposit_iface), deferred :: deposit
    end type
    abstract interface
        integer function balance_iface(self)
            import :: IAccount
            class(IAccount), intent(in) :: self
        end function
        subroutine deposit_iface(self, amount)
            import :: IAccount
            class(IAccount), intent(inout) :: self
            integer, intent(in) :: amount
        end subroutine
    end interface
    type, extends(IAccount) :: Account
        integer :: n = 7
    contains
        procedure :: balance => account_balance
        procedure :: deposit => account_deposit
    end type
    type, extends(IAccount) :: Wallet
        integer :: cents = 100
    contains
        procedure :: balance => wallet_balance
        procedure :: deposit => wallet_deposit
    end type
    interface
        module integer function account_balance(self)
            class(Account), intent(in) :: self
        end function
        module subroutine account_deposit(self, amount)
            class(Account), intent(inout) :: self
            integer, intent(in) :: amount
        end subroutine
        module integer function wallet_balance(self)
            class(Wallet), intent(in) :: self
        end function
        module subroutine wallet_deposit(self, amount)
            class(Wallet), intent(inout) :: self
            integer, intent(in) :: amount
        end subroutine
    end interface
contains
    integer function observe(x)
        class(IAccount), intent(in) :: x
        observe = x%balance()
    end function
    subroutine credit(x, amount)
        class(IAccount), intent(inout) :: x
        integer, intent(in) :: amount
        call x%deposit(amount)
    end subroutine
end module

submodule (traits_submodule_binding_01_oracle_m) traits_submodule_binding_01_oracle_s
    implicit none
contains
    module procedure account_balance
        account_balance = self%n
    end procedure
    module procedure account_deposit
        self%n = self%n + amount
    end procedure
    module integer function wallet_balance(self)
        class(Wallet), intent(in) :: self
        wallet_balance = self%cents
    end function
    module subroutine wallet_deposit(self, amount)
        class(Wallet), intent(inout) :: self
        integer, intent(in) :: amount
        self%cents = self%cents + 100*amount
    end subroutine
end submodule

module traits_submodule_binding_01_oracle_client
    use traits_submodule_binding_01_oracle_m
    implicit none
contains
    integer function total(x, y)
        class(IAccount), intent(in) :: x, y
        total = x%balance() + y%balance()
    end function
end module

module traits_submodule_binding_01_oracle_local
    implicit none
    type, abstract :: IVal
    contains
        procedure(get_iface), deferred :: get
    end type
    abstract interface
        integer function get_iface(self)
            import :: IVal
            class(IVal), intent(in) :: self
        end function
    end interface
    type, extends(IVal) :: Cell
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

submodule (traits_submodule_binding_01_oracle_local) traits_submodule_binding_01_oracle_local_s
contains
    module procedure cell_get
        cell_get = 10*self%n
    end procedure
end submodule

program traits_submodule_binding_01_oracle
    use traits_submodule_binding_01_oracle_m
    use traits_submodule_binding_01_oracle_client
    use traits_submodule_binding_01_oracle_local
    implicit none
    type(Account) :: a
    type(Wallet) :: w
    type(Cell) :: c
    class(IAccount), allocatable :: owner

    if (a%balance() /= 7) error stop 1
    call a%deposit(2)
    if (a%balance() /= 9) error stop 2
    if (w%balance() /= 100) error stop 3
    call w%deposit(1)
    if (w%balance() /= 200) error stop 4
    if (c%get() /= 30) error stop 5
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
    call a%deposit(10)
    if (a%balance() /= 20) error stop 11
    call w%deposit(1)
    if (w%balance() /= 500) error stop 12
    print '(a)', 'traits_submodule_binding_01_oracle ok'
end program
