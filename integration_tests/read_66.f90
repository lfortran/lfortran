! Test reading into arrays with stride > 1 and stride = 1, for various types.
program read_66
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none

    call test_int8()
    call test_int16()
    call test_int32()
    call test_int64()
    call test_real32()
    call test_real64()
    call test_logical4()
    call test_complex32()
    call test_complex64()

contains

    subroutine test_int8()
        integer(int8) :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = -99_int8
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (1_int8, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 int8 stride>1 iostat"
        if (.not. all(a(1, :) == 1_int8)) error stop "read_66 int8 stride>1 value"

        a = -99_int8
        rewind(u)
        write(u, *) (2_int8, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 int8 stride=1 iostat"
        if (.not. all(a(:, 1) == 2_int8)) error stop "read_66 int8 stride=1 value"
    end subroutine test_int8

    subroutine test_int16()
        integer(int16) :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = -999_int16
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (3_int16, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 int16 stride>1 iostat"
        if (.not. all(a(1, :) == 3_int16)) error stop "read_66 int16 stride>1 value"

        a = -999_int16
        rewind(u)
        write(u, *) (4_int16, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 int16 stride=1 iostat"
        if (.not. all(a(:, 1) == 4_int16)) error stop "read_66 int16 stride=1 value"
    end subroutine test_int16

    subroutine test_int32()
        integer(int32) :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = -999_int32
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (5_int32, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 int32 stride>1 iostat"
        if (.not. all(a(1, :) == 5_int32)) error stop "read_66 int32 stride>1 value"

        a = -999_int32
        rewind(u)
        write(u, *) (6_int32, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 int32 stride=1 iostat"
        if (.not. all(a(:, 1) == 6_int32)) error stop "read_66 int32 stride=1 value"
    end subroutine test_int32

    subroutine test_int64()
        integer(int64) :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = -999_int64
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (7_int64, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 int64 stride>1 iostat"
        if (.not. all(a(1, :) == 7_int64)) error stop "read_66 int64 stride>1 value"

        a = -999_int64
        rewind(u)
        write(u, *) (8_int64, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 int64 stride=1 iostat"
        if (.not. all(a(:, 1) == 8_int64)) error stop "read_66 int64 stride=1 value"
    end subroutine test_int64

    subroutine test_real32()
        real(real32) :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = -999.0_real32
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (1.0_real32, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 real32 stride>1 iostat"
        if (.not. all(a(1, :) == 1.0_real32)) error stop "read_66 real32 stride>1 value"

        a = -999.0_real32
        rewind(u)
        write(u, *) (2.0_real32, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 real32 stride=1 iostat"
        if (.not. all(a(:, 1) == 2.0_real32)) error stop "read_66 real32 stride=1 value"
    end subroutine test_real32

    subroutine test_real64()
        real(real64) :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = -999.0_real64
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (3.0_real64, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 real64 stride>1 iostat"
        if (.not. all(a(1, :) == 3.0_real64)) error stop "read_66 real64 stride>1 value"

        a = -999.0_real64
        rewind(u)
        write(u, *) (4.0_real64, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 real64 stride=1 iostat"
        if (.not. all(a(:, 1) == 4.0_real64)) error stop "read_66 real64 stride=1 value"
    end subroutine test_real64

    subroutine test_logical4()
        logical :: a(10, 10)
        integer :: i, u, ios
        character(len=200) :: msg

        a = .false.
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (.true., i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 logical stride>1 iostat"
        if (.not. all(a(1, :))) error stop "read_66 logical stride>1 value"

        a = .false.
        rewind(u)
        write(u, *) (.true., i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 logical stride=1 iostat"
        if (.not. all(a(:, 1))) error stop "read_66 logical stride=1 value"
    end subroutine test_logical4

    subroutine test_complex32()
        complex(real32) :: a(10, 10)
        complex(real32) :: v1, v2
        integer :: i, u, ios
        character(len=200) :: msg

        v1 = cmplx(1.0_real32, 2.0_real32, kind=real32)
        v2 = cmplx(3.0_real32, 4.0_real32, kind=real32)
        a = cmplx(-1.0_real32, -1.0_real32, kind=real32)
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (v1, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 complex32 stride>1 iostat"
        if (.not. all(a(1, :) == v1)) error stop "read_66 complex32 stride>1 value"

        a = cmplx(-1.0_real32, -1.0_real32, kind=real32)
        rewind(u)
        write(u, *) (v2, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 complex32 stride=1 iostat"
        if (.not. all(a(:, 1) == v2)) error stop "read_66 complex32 stride=1 value"
    end subroutine test_complex32

    subroutine test_complex64()
        complex(real64) :: a(10, 10)
        complex(real64) :: v1, v2
        integer :: i, u, ios
        character(len=200) :: msg

        v1 = cmplx(5.0_real64, 6.0_real64, kind=real64)
        v2 = cmplx(7.0_real64, 8.0_real64, kind=real64)
        a = cmplx(-1.0_real64, -1.0_real64, kind=real64)
        open(newunit=u, status="scratch", form="formatted")
        write(u, *) (v1, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(1, :)
        if (ios /= 0) error stop "read_66 complex64 stride>1 iostat"
        if (.not. all(a(1, :) == v1)) error stop "read_66 complex64 stride>1 value"

        a = cmplx(-1.0_real64, -1.0_real64, kind=real64)
        rewind(u)
        write(u, *) (v2, i=1, 10)
        rewind(u)
        read(u, *, iostat=ios, iomsg=msg) a(:, 1)
        close(u)
        if (ios /= 0) error stop "read_66 complex64 stride=1 iostat"
        if (.not. all(a(:, 1) == v2)) error stop "read_66 complex64 stride=1 value"
    end subroutine test_complex64

end program read_66
