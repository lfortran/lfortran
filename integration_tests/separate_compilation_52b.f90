subroutine check_common_chars()
    implicit none
    ! The same 4 bytes of common block /nm_52/ are declared here as an array of
    ! four single characters (storage association). This only works if the
    ! common character storage lives inline in the block, shared across the
    ! separately compiled units, rather than behind a per-unit descriptor.
    character(1) :: ch(4)
    common /nm_52/ ch
    if (ch(1) /= "A") error stop "ch(1)"
    if (ch(2) /= "B") error stop "ch(2)"
    if (ch(3) /= "C") error stop "ch(3)"
    if (ch(4) /= "D") error stop "ch(4)"
end subroutine check_common_chars
