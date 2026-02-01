! Test for https://github.com/lfortran/lfortran/issues/8201
! Non-default I/O kind with selected_char_kind and encoding
program intrinsics_415
    use iso_fortran_env, only: output_unit
    implicit none
    integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
    integer, parameter :: utf8 = selected_char_kind('UTF-8')

    if (ucs4 < 0) error stop "ISO_10646 should be supported"
    if (utf8 >= 0) error stop "UTF-8 should not be supported"

    print *, "PASSED: selected_char_kind for ISO_10646 and UTF-8"
end program intrinsics_415
