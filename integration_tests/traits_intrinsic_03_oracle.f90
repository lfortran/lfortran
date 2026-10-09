module traits_intrinsic_03_oracle_m
    use iso_fortran_env, only: int64, real64
    implicit none
    interface read_value
        module procedure int64_value, complex8_value, logical_value, logical8_value
    end interface
contains
    integer function int64_value(self) result(n)
        integer(int64), intent(in) :: self
        n = int(self)
    end function
    integer function complex8_value(self) result(n)
        complex(8), intent(in) :: self
        n = int(real(self, real64)) + int(aimag(self))
    end function
    integer function logical_value(self) result(n)
        logical, intent(in) :: self
        n = 0
        if (self) n = 1
    end function
    integer function logical8_value(self) result(n)
        logical(8), intent(in) :: self
        n = 0
        if (self) n = 1
    end function
end module

program traits_intrinsic_03_oracle
    use traits_intrinsic_03_oracle_m
    implicit none
    integer(int64) :: i8
    complex(8) :: z8
    logical :: ld
    logical(8) :: l8
    i8 = 42_int64
    z8 = cmplx(3.0_real64, -2.0_real64, kind=8)
    ld = .true.
    l8 = .true.
    if (int64_value(i8) /= 42) error stop 1
    if (read_value(i8) /= 42) error stop 2
    if (int64_value(i8) /= 42) error stop 3
    if (complex8_value(z8) /= 1) error stop 4
    if (read_value(z8) /= 1) error stop 5
    if (complex8_value(z8) /= 1) error stop 6
    if (logical_value(ld) /= 1 .or. read_value(ld) /= 1) error stop 7
    if (logical8_value(l8) /= 1 .or. read_value(l8) /= 1) error stop 8
    ld = .false.
    l8 = .false.
    if (read_value(ld) /= 0 .or. read_value(l8) /= 0) error stop 9
end program
