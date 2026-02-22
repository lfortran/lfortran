! Test for https://github.com/lfortran/lfortran/issues/8201
! Non-default I/O kind with selected_char_kind and encoding
program intrinsics_417
    use iso_fortran_env, only: output_unit
    implicit none
    integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
    integer, parameter :: utf8 = selected_char_kind('UTF-8')

    if (ucs4 < 0) error stop "ISO_10646 should be supported"

    ! The actual issue was that open with encoding='UTF-8' failed with:
    ! "semantic error: Invalid argument `encoding` supplied"
    open(output_unit, encoding='UTF-8')

    print *, "PASSED: open with encoding and selected_char_kind"
end program intrinsics_417
