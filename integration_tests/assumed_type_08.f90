! Character actuals passed through type(*) wrappers to a bind(C) type(*),
! dimension(..) routine: the CFI descriptor's base_addr is the address of
! the actual's character data and elem_len is its length in bytes.
module assumed_type_08_mod
    use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t, c_int32_t
    implicit none
    interface
        subroutine check_char_cfi(d, expected, elem_len, rank, ok) bind(C)
            import :: c_ptr, c_size_t, c_int32_t
            type(*), dimension(..) :: d
            type(c_ptr), value :: expected
            integer(c_size_t), value :: elem_len
            integer(c_int32_t), value :: rank
            integer(c_int32_t), intent(out) :: ok
        end subroutine
    end interface
contains

    integer(c_int32_t) function fwd_rank(buf, expected, elem_len, rank)
        type(*), dimension(..) :: buf
        type(c_ptr), intent(in) :: expected
        integer(c_size_t), intent(in) :: elem_len
        integer(c_int32_t), intent(in) :: rank
        call check_char_cfi(buf, expected, elem_len, rank, fwd_rank)
    end function

    ! elem_len = 0: only base_addr and rank are checked
    integer(c_int32_t) function fwd_shape(buf, expected)
        type(*), dimension(:) :: buf
        type(c_ptr), intent(in) :: expected
        call check_char_cfi(buf, expected, 0_c_size_t, 1_c_int32_t, fwd_shape)
    end function

end module

program assumed_type_08
    use, intrinsic :: iso_c_binding, only: c_loc, c_size_t, c_int32_t
    use assumed_type_08_mod
    implicit none
    character(len=7), target :: s
    character(len=3), target :: a(4), b(4)

    s = 'abcdefg'
    if (fwd_rank(s, c_loc(s), 7_c_size_t, 0_c_int32_t) /= 1) error stop 1
    if (s /= 'Ybcdefg') error stop 2

    a = ['abc', 'def', 'ghi', 'jkl']
    if (fwd_rank(a, c_loc(a), 3_c_size_t, 1_c_int32_t) /= 1) error stop 3
    if (a(1) /= 'Ybc' .or. a(4) /= 'jkl') error stop 4

    b = ['abc', 'def', 'ghi', 'jkl']
    if (fwd_shape(b, c_loc(b)) /= 1) error stop 5
    if (b(1) /= 'Ybc' .or. b(4) /= 'jkl') error stop 6

    print *, 'ok'
end program
