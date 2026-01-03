program format_36
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none

    real(real32) :: val32
    real(real64) :: val64
    integer(int64) :: minus_one
    integer(int64) :: val64_bits
    character(len=8)  :: hex32
    character(len=16) :: hex64_real
    character(len=16) :: hex64_expected
    character(len=16) :: hex64_int

    val32 = 123.456_real32
    val64 = 123.456_real64
    minus_one = -1_int64
    val64_bits = transfer(val64, val64_bits)

    write(hex32, '(Z8.8)') val32
    if (trim(hex32) /= '42F6E979') error stop

    write(hex64_real, '(Z16.16)') val64
    write(hex64_expected, '(Z16.16)') val64_bits
    if (trim(hex64_real) /= trim(hex64_expected)) error stop

    write(hex64_int, '(Z16.16)') minus_one
    if (trim(hex64_int) /= 'FFFFFFFFFFFFFFFF') error stop
end program format_36
