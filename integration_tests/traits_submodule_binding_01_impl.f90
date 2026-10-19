! Submodule of traits_submodule_binding_01.f90: the bodies of the separate
! module procedures that witness IAccount, in both the module procedure and
! the full-interface forms. The program also uses the client module below,
! so that build systems compile this file before the program, whose
! compilation without --separate-compilation loads the submodule.
submodule (traits_submodule_binding_01_m) traits_submodule_binding_01_s
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

module traits_submodule_binding_01_client
    use traits_submodule_binding_01_m
    implicit none
contains
    integer function total(x, y)
        class(IAccount), intent(in) :: x, y
        total = x%balance() + y%balance()
    end function
end module
