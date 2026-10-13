! Standard-Fortran oracle for traits_intrinsic_02.f90.
module traits_intrinsic_02_oracle_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
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

    function read_integer(x) result(res)
        integer, intent(in) :: x
        integer :: res
        res = integer_value(x)
    end function read_integer

    function read_real64(x) result(res)
        real(real64), intent(in) :: x
        integer :: res
        res = real64_value(x)
    end function read_real64

    function facade_read_integer(x) result(res)
        integer, intent(in) :: x
        integer :: res
        res = read_integer(x) + 1
    end function facade_read_integer

    function facade_read_real64(x) result(res)
        real(real64), intent(in) :: x
        integer :: res
        res = read_real64(x) + 1
    end function facade_read_real64

end module traits_intrinsic_02_oracle_m

program traits_intrinsic_02_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_intrinsic_02_oracle_m
    implicit none

    integer :: i
    real(real64) :: r8

    i = 5
    r8 = 2.5_real64

    if (read_integer(i) /= 15) error stop 201
    if (read_real64(r8) /= 22) error stop 202
    if (facade_read_integer(i) /= 16) error stop 203
    if (facade_read_real64(r8) /= 23) error stop 204
end program traits_intrinsic_02_oracle
