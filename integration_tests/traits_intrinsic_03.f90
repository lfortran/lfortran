module traits_intrinsic_03_m
    use iso_fortran_env, only: int64, real64
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    implements IValue :: integer(int64)
        procedure :: value => int64_value
    end implements integer(int64)
    implements IValue :: complex(8)
        procedure :: value => complex8_value
    end implements complex(8)
    implements IValue :: logical
        procedure :: value => logical_value
    end implements logical
    implements IValue :: logical(kind=8)
        procedure :: value => logical8_value
    end implements logical(kind=8)
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
    function read_value{IValue :: T}(x) result(n)
        type(T), intent(in) :: x
        integer :: n
        n = x%value()
    end function
end module

program traits_intrinsic_03
    use traits_intrinsic_03_m
    implicit none
    integer(int64) :: i8
    complex(8) :: z8
    logical :: ld
    logical(8) :: l8
    i8 = 42_int64
    z8 = cmplx(3.0_real64, -2.0_real64, kind=8)
    ld = .true.
    l8 = .true.
    if (i8%value() /= 42) error stop 1
    if (read_value(i8) /= 42) error stop 2
    if (read_value{integer(int64)}(i8) /= 42) error stop 3
    if (z8%value() /= 1) error stop 4
    if (read_value(z8) /= 1) error stop 5
    if (read_value{complex(8)}(z8) /= 1) error stop 6
    if (ld%value() /= 1 .or. read_value(ld) /= 1) error stop 7
    if (l8%value() /= 1 .or. read_value{logical(8)}(l8) /= 1) error stop 8
    ld = .false.
    l8 = .false.
    if (read_value(ld) /= 0 .or. read_value(l8) /= 0) error stop 9
end program
