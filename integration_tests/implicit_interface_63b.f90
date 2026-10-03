! Companion to implicit_interface_63, compiled as a separate unit so that the
! caller has to synthesize an implicit interface for each of these.
subroutine check_fixed_shape(arr)
    implicit none
    character(len=8) :: arr(3)
    if (len(arr(1)) /= 8) error stop 11
    if (arr(1) /= 'ab') error stop 12
    if (arr(2) /= 'cd') error stop 13
    if (arr(3) /= 'ef') error stop 14
end subroutine check_fixed_shape

subroutine check_star_shape(arr)
    implicit none
    character(len=*) :: arr(3)
    if (len(arr(1)) /= 8) error stop 21
    if (arr(1) /= 'ab') error stop 22
    if (arr(2) /= 'cd') error stop 23
    if (arr(3) /= 'ef') error stop 24
end subroutine check_star_shape

subroutine check_fixed_size(arr)
    implicit none
    character(len=8) :: arr(*)
    if (len(arr(1)) /= 8) error stop 31
    if (arr(1) /= 'ab') error stop 32
    if (arr(2) /= 'cd') error stop 33
    if (arr(3) /= 'ef') error stop 34
end subroutine check_fixed_size

subroutine check_star_size(arr)
    implicit none
    character(len=*) :: arr(*)
    if (len(arr(1)) /= 8) error stop 41
    if (arr(1) /= 'ab') error stop 42
    if (arr(2) /= 'cd') error stop 43
    if (arr(3) /= 'ef') error stop 44
end subroutine check_star_size

! The LAPACK pattern from issue #9381.
subroutine check_char1_size(arr)
    implicit none
    character(1) :: arr(*)
    if (len(arr(1)) /= 1) error stop 51
    if (arr(1) /= 'a') error stop 52
    if (arr(2) /= 'c') error stop 53
    if (arr(3) /= 'e') error stop 54
end subroutine check_char1_size

subroutine check_text16(text, n)
    implicit none
    integer :: n
    character(16) :: text(n)
    if (len(text(1)) /= 16) error stop 61
    if (n /= 4) error stop 62
    if (text(1) /= 'TEXT1') error stop 63
    if (text(3) /= 'TEXT3') error stop 64
    if (text(4) /= 'TEXT4') error stop 65
end subroutine check_text16

subroutine check_rank2(g)
    implicit none
    character(len=4) :: g(*)
    if (len(g(1)) /= 4) error stop 71
    if (g(1) /= 'a11') error stop 72
    if (g(2) /= 'b21') error stop 73
    if (g(4) /= 'd22') error stop 74
    if (g(6) /= 'f23') error stop 75
end subroutine check_rank2

subroutine check_section(arr)
    implicit none
    character(len=*) :: arr(*)
    if (len(arr(1)) /= 8) error stop 81
    if (arr(1) /= 'cd') error stop 82
    if (arr(2) /= 'ef') error stop 83
end subroutine check_section
