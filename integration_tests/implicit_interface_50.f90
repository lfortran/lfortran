! Passing a CHARACTER array actual to an external INTEGER FUNCTION reached
! through an implicit interface. This mirrors netcdf-fortran's nf_test4
! (ftst_vars6.F, ftst_var_szip.F, ftst_var_compact.F), whose check_file()
! is an INTEGER FUNCTION taking a `character*(4) var_name(NVARS)` array.
!
! Through an implicit interface the caller must pass the character array by the
! classic Fortran hidden-length ABI: the contiguous element data pointer at the
! argument position and the (per-element) length as a hidden trailing argument.
! LFortran previously wrapped the array in an array descriptor and passed a
! pointer to it, so the callee -- which expects a { data, len } string
! descriptor -- reinterpreted the array-descriptor bytes as character data and
! saw garbage. A CHARACTER scalar, or the same array passed to a SUBROUTINE,
! both worked; only the INTEGER-FUNCTION + CHARACTER-array combination failed.
program implicit_interface_50
    implicit none
    character(4) :: var_name(2)
    character(10) :: words(3)
    integer :: check_arr, check_words, retval

    var_name(1) = 'var1'
    var_name(2) = 'var2'
    retval = check_arr(var_name)
    if (retval /= 0) error stop

    words(1) = 'First     '
    words(2) = 'Second    '
    words(3) = 'Third     '
    retval = check_words(words)
    if (retval /= 6) error stop

    print *, "OK"
end program implicit_interface_50

integer function check_arr(var_name)
    implicit none
    character(4) :: var_name(2)
    check_arr = 0
    if (var_name(1) /= 'var1') check_arr = check_arr + 1
    if (var_name(2) /= 'var2') check_arr = check_arr + 2
end function check_arr

integer function check_words(words)
    implicit none
    character(*) :: words(*)
    check_words = 0
    if (words(1)(1:5) == 'First') check_words = check_words + 1
    if (words(2)(1:6) == 'Second') check_words = check_words + 2
    if (words(3)(1:5) == 'Third') check_words = check_words + 3
end function check_words
