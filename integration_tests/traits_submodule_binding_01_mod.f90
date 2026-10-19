! Module of traits_submodule_binding_01.f90: its trait witnesses are separate
! module procedures, declared here and implemented in the submodule of
! traits_submodule_binding_01_impl.f90.
module traits_submodule_binding_01_m
    implicit none
    abstract interface :: IAccount
        integer function balance(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine deposit(self, amount)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: amount
        end subroutine
    end interface
    ! Witnesses bound in the type definition.
    type, implements(IAccount) :: Account
        integer :: n = 7
    contains
        procedure :: balance => account_balance
        procedure :: deposit => account_deposit
    end type
    ! Witnesses bound in an implements block.
    type :: Wallet
        integer :: cents = 100
    end type
    implements IAccount :: Wallet
        procedure, pass :: balance => wallet_balance
        procedure, pass :: deposit => wallet_deposit
    end implements
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
    integer function read_generic{IAccount :: T}(x)
        type(T), intent(in) :: x
        read_generic = x%balance()
    end function
    subroutine credit_generic{IAccount :: T}(x, amount)
        type(T), intent(inout) :: x
        integer, intent(in) :: amount
        call x%deposit(amount)
    end subroutine
end module
