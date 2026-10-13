! Standard-Fortran counterpart of traits_runtime_numeric_separate_01_contracts.f90.
! Abstract types replace traits; one deferred specific per member of
! integer | real(real64) behind a type-bound GENERIC binding.
module traits_runtime_numeric_separate_01_oracle_contracts_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    type, abstract :: ISum
    contains
        procedure(sum_integer_interface), deferred :: sum_integer
        procedure(sum_real64_interface), deferred :: sum_real64
        generic :: sum => sum_integer, sum_real64
    end type ISum

    type, abstract :: IAverager
    contains
        procedure(average_integer_interface), deferred :: average_integer
        procedure(average_real64_interface), deferred :: average_real64
        generic :: average => average_integer, average_real64
    end type IAverager

    abstract interface
        function sum_integer_interface(self, x) result(s)
            import :: ISum
            class(ISum), intent(in) :: self
            integer,     intent(in) :: x(:)
            integer                 :: s
        end function sum_integer_interface

        function sum_real64_interface(self, x) result(s)
            import :: ISum, real64
            class(ISum),  intent(in) :: self
            real(real64), intent(in) :: x(:)
            real(real64)             :: s
        end function sum_real64_interface

        function average_integer_interface(self, x) result(a)
            import :: IAverager
            class(IAverager), intent(in) :: self
            integer,          intent(in) :: x(:)
            integer                      :: a
        end function average_integer_interface

        function average_real64_interface(self, x) result(a)
            import :: IAverager, real64
            class(IAverager), intent(in) :: self
            real(real64),     intent(in) :: x(:)
            real(real64)                 :: a
        end function average_real64_interface
    end interface

    ! Implemented by the separately compiled provider; clients need only this
    ! contract module.
    interface
        subroutine make_sum(choice, object)
            import :: ISum
            integer, intent(in) :: choice
            class(ISum), allocatable, intent(out) :: object
        end subroutine make_sum

        subroutine make_averager(choice, object)
            import :: IAverager
            integer, intent(in) :: choice
            class(IAverager), allocatable, intent(out) :: object
        end subroutine make_averager
    end interface
end module traits_runtime_numeric_separate_01_oracle_contracts_m
