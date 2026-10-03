program transfer_24
    use iso_fortran_env, only: int8, int64, real32
    implicit none

    integer(int8), target :: key(8)
    integer(int8), pointer :: key_ptr(:)
    integer(int64) :: c_ptr, c_arr

    real(real32), target :: rvals(2)
    real(real32), pointer :: rptr(:)
    integer(int64) :: r_bits_ptr, r_bits_arr

    character(1), target :: chars(8)
    character(1), pointer :: chars_ptr(:)
    integer(int64) :: ch_bits_ptr, ch_bits_arr

    key = [ -6_int8, -40_int8, 1_int8, 0_int8, -12_int8, -79_int8, 3_int8, 0_int8 ]
    key_ptr => key

    c_ptr = 1_int64 + transfer(key_ptr, 0_int64)
    c_arr = 1_int64 + transfer(key, 0_int64)
    if (c_ptr /= c_arr) error stop

    rvals = [ 1.25_real32, -2.75_real32 ]
    rptr => rvals

    r_bits_ptr = transfer(rptr, 0_int64)
    r_bits_arr = transfer(rvals, 0_int64)
    if (r_bits_ptr /= r_bits_arr) error stop

    chars = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H' ]
    chars_ptr => chars

    ch_bits_ptr = transfer(chars_ptr, 0_int64)
    ch_bits_arr = transfer(chars, 0_int64)
    if (ch_bits_ptr /= ch_bits_arr) error stop

end program transfer_24
