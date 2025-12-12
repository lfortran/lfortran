! MRE from LAPACK stfsm.f: 0-based assumed-size dummy A(0:*)
! Pattern: sequence association when passing A(m) to X(*)
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
    ! Sequence association: pass element A(M) to assumed-size dummy X(*)
    call sub(A(M))
end subroutine

subroutine sub(X)
    implicit none
    real :: X(*)
    if (X(1) /= 1.0) error stop "Wrong value"
end subroutine
