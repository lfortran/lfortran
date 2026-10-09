! Standard-Fortran oracle for traits_intrinsic_01.f90.
module traits_intrinsic_01_oracle_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
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

    function real64_adjust(raw, delta) result(res)
        real(real64), intent(in) :: raw
        integer, intent(in) :: delta
        real(real64) :: res
        res = raw + real(delta, real64)
    end function real64_adjust

    subroutine clear_integer(out)
        integer, intent(out) :: out
        out = 0
    end subroutine clear_integer

    function read_integer(x) result(res)
        integer, intent(in) :: x
        integer :: res
        res = integer_value(x)
    end function read_integer

    function read_real32(x) result(res)
        real(real32), intent(in) :: x
        integer :: res
        res = real32_value(x)
    end function read_real32

    function read_real64(x) result(res)
        real(real64), intent(in) :: x
        integer :: res
        res = real64_value(x)
    end function read_real64

end module traits_intrinsic_01_oracle_m

program traits_intrinsic_01_oracle
    use traits_intrinsic_01_oracle_m
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
    if (integer_value(i) /= 107) error stop 103
    if (real32_value(r4) /= 202) error stop 104
    if (real64_value(r8) /= 304) error stop 105
    if (read_integer(i) /= 107) error stop 106
    if (read_real32(r4) /= 202) error stop 107
    if (read_real64(r8) /= 304) error stop 108

    if (integer_shift(i, 3) /= 37) error stop 109
    if (integer_shift(i, delta=3) /= 37) error stop 110
    if (abs(real64_adjust(r8, 2) - 6.9_real64) > 32.0_real64*epsilon(r8)) error stop 111

    call clear_integer(cleared)
    if (cleared /= 0) error stop 112

    call output(r8)
end program traits_intrinsic_01_oracle
