module traits_intrinsic_02_provider_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_intrinsic_02_contracts_m, only: IValue
    implicit none

    implements IValue :: integer
        procedure, pass :: value => integer_value
    end implements integer

    implements IValue :: real(real64)
        procedure, pass :: value => real64_value
    end implements real(real64)

contains

    function integer_value(self) result(res)
        integer, intent(in) :: self
        integer :: res
        res = self + 10
    end function integer_value

    function real64_value(self) result(res)
        real(real64), intent(in) :: self
        integer :: res
        res = int(self) + 20
    end function real64_value

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%value()
    end function read_value

end module traits_intrinsic_02_provider_m
