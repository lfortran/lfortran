! MRE from LAPACK stfsm.f: ArraySection on 0-based assumed-size array
! Pattern: A(0:*) sliced as A(m:) triggers ArrayBound in codegen
program lapack_06
    implicit none
    real :: A(0:10)
    A = 1.0
    call stfsm(A)
    print *, "PASS"
end program

subroutine stfsm(A)
    implicit none
    real :: A(0:*)
    integer :: M
    M = 5
    ! This triggers ArraySection with ArrayBound on assumed-size
    call sub(A(M:))
end subroutine

subroutine sub(X)
    implicit none
    real :: X(*)
    if (X(1) /= 1.0) error stop "Wrong value"
end subroutine
