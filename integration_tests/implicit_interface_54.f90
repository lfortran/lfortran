! Passing a CHARACTER array actual to a scalar CHARACTER(len=*) dummy of an
! external procedure reached through an implicit interface. This mirrors
! netcdf-fortran's nf_get_var_text (fortran/nf_vario.F90), an INTEGER FUNCTION
! whose `text` dummy is CHARACTER(len=*), intent(out), reached from the F77
! nf_test callers (nf_test/test_get.F) with a CHARACTER*1 array actual.
!
! Through an implicit interface the caller must pass the character array by the
! classic Fortran hidden-length ABI: the contiguous element data pointer at the
! argument position and the per-element length as a hidden trailing argument.
! LFortran previously omitted the trailing length for a fixed-length CHARACTER
! array actual (only assumed/deferred-length arrays carried it), so the
! separately compiled scalar CHARACTER(len=*) callee read a garbage length and
! crashed while blanking the string (text = ' ' -> _lfortran_copy_str_and_pad
! -> memset). Every hidden-length CHARACTER dummy now uniformly carries the
! trailing length, matching gfortran/flang.
!
! get_text is defined in implicit_interface_54b.f90 and compiled separately, so
! the caller here sees only the implicit interface (no hidden length is implied
! by any explicit interface). It returns LEN(text) -- the per-element length of
! the actual -- and blanks the string, exactly the crash site.
program implicit_interface_54
    implicit none
    integer :: get_text
    character :: value(8)      ! CHARACTER*1 array, like nf_test's `value`
    character(4) :: words(3)   ! fixed-length CHARACTER*4 array
    integer :: err

    value = 'Z'
    err = get_text(value)
    if (err /= 1) error stop        ! callee saw the element length (1)
    if (value(1) /= ' ') error stop ! first element blanked
    if (value(2) /= 'Z') error stop ! sequence association: rest untouched

    words = 'zzzz'
    err = get_text(words)
    if (err /= 4) error stop            ! callee saw the element length (4)
    if (words(1) /= '    ') error stop  ! first element blanked
    if (words(2) /= 'zzzz') error stop  ! sequence association: rest untouched

    print *, "OK"
end program implicit_interface_54
