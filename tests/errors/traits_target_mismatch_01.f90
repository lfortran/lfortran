! The TARGET, CONTIGUOUS and VOLATILE attributes of a dummy argument of an
! implementation must match those of the trait method.
module traits_target_mismatch_01_array
    implicit none
    abstract interface :: ITargetTrait
        subroutine method(self, a)
            type(itself), pass, intent(inout) :: self
            real(8), target, intent(in) :: a(:)
        end subroutine method
    end interface

    type, implements(ITargetTrait) :: Keeper  ! {Error} target, contiguous or volatile attribute of argument 'a' does not match trait method 'method'
    contains
        procedure :: method => keeper_method
    end type Keeper
contains
    subroutine keeper_method(self, a)
        class(Keeper), intent(inout) :: self
        real(8), intent(in) :: a(:)
    end subroutine keeper_method
end module traits_target_mismatch_01_array

module traits_target_mismatch_01_scalar
    implicit none
    abstract interface :: IScalar
        subroutine method(self, n)
            type(itself), pass, intent(inout) :: self
            integer, intent(in) :: n
        end subroutine method
    end interface

    type, implements(IScalar) :: Holder  ! {Error} target, contiguous or volatile attribute of argument 'n' does not match trait method 'method'
    contains
        procedure :: method => holder_method
    end type Holder
contains
    subroutine holder_method(self, n)
        class(Holder), intent(inout) :: self
        integer, volatile, intent(in) :: n
    end subroutine holder_method
end module traits_target_mismatch_01_scalar
