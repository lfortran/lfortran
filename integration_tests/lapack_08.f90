! MRE: COMMON block + multiple EQUIVALENCE pairs causes symtab error
! When a subroutine has both COMMON and multiple EQUIVALENCE pairs,
! the ASR verifier reports "symbol table was not found in scope"
! Reduced from LAPACK sblat1.f CHECK2 subroutine
program lapack_08
    integer :: n
    common /c/ n
    call sub()
    print *, 'PASS'
end program

subroutine sub()
    integer :: n
    real :: x(8), a(4), b(4)
    common /c/ n
    equivalence (x(1), a(1)), (x(5), b(1))
end subroutine
