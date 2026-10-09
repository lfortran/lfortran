! Test-first extension fixture for intrinsic static trait implementations.
! The provider is intentionally imported with a full USE by the client.
module traits_intrinsic_01_provider_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue

    abstract interface :: IPrintable
        subroutine output()
        end subroutine output
    end interface IPrintable

    abstract interface :: IShift
        function shift(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shift
    end interface IShift

    abstract interface :: IAdjust
        function adjust(delta) result(res)
            integer, intent(in) :: delta
            real(real64) :: res
        end function adjust
    end interface IAdjust

    abstract interface :: IClear
        subroutine clear(out)
            integer, intent(out) :: out
        end subroutine clear
    end interface IClear

    implements IValue :: integer
        procedure, pass :: value => integer_value
    end implements integer

    implements IValue :: real(real32)
        procedure, pass :: value => real32_value
    end implements real(real32)

    implements IValue :: real(real64)
        procedure, pass :: value => real64_value
    end implements real(real64)

    implements IPrintable :: real(real64)
        procedure, pass :: output
    end implements real(real64)

    implements IShift :: integer
        procedure, pass :: shift => integer_shift
    end implements integer

    implements IAdjust :: real(real64)
        procedure, pass(raw) :: adjust => real64_adjust
    end implements real(real64)

    implements IClear :: integer
        procedure, nopass :: clear => clear_integer
    end implements integer

contains

    function integer_value(self) result(res)
        integer, intent(in) :: self
        integer :: res
        res = self + 100
    end function integer_value

    function real32_value(self) result(res)
        real(real32), intent(in) :: self
        integer :: res
        res = int(self) + 200
    end function real32_value

    function real64_value(self) result(res)
        real(real64), intent(in) :: self
        integer :: res
        res = int(self) + 300
    end function real64_value

    subroutine output(self)
        real(real64), intent(in) :: self
        write(*,*) "I am ", self
    end subroutine output

    function integer_shift(self, delta) result(res)
        integer, intent(in) :: self
        integer, intent(in) :: delta
        integer :: res
        res = self + 10*delta
    end function integer_shift

    function real64_adjust(delta, raw) result(res)
        integer, intent(in) :: delta
        real(real64), intent(in) :: raw
        real(real64) :: res
        res = raw + real(delta, real64)
    end function real64_adjust

    subroutine clear_integer(out)
        integer, intent(out) :: out
        out = 0
    end subroutine clear_integer

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%value()
    end function read_value

end module traits_intrinsic_01_provider_m

program traits_intrinsic_01
    use traits_intrinsic_01_provider_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none

    integer :: i, cleared
    real(real32) :: r4
    real(real64) :: r8

    i = 7
    r4 = 2.5_real32
    r8 = 4.9_real64

    if (kind(r4) /= real32) error stop 101
    if (kind(r8) /= real64) error stop 102
    if (i%value() /= 107) error stop 103
    if (r4%value() /= 202) error stop 104
    if (r8%value() /= 304) error stop 105
    if (read_value(i) /= 107) error stop 106
    if (read_value(r4) /= 202) error stop 107
    if (read_value{real(real64)}(r8) /= 304) error stop 108

    if (i%shift(3) /= 37) error stop 109
    if (i%shift(delta=3) /= 37) error stop 110
    if (abs(r8%adjust(2) - 6.9_real64) > 32.0_real64*epsilon(r8)) error stop 111

    call i%clear(cleared)
    if (cleared /= 0) error stop 112

    ! This is the paper-printy static call, after an explicit provider USE.
    call r8%output()
end program traits_intrinsic_01
