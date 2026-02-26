subroutine ilu0 (M,JA)
    implicit none
    integer :: JA(M)
    integer :: M
    print *, sum(JA) * M
    if (sum(JA) * M /= 900) error stop
end subroutine

program array_dimension_01
    implicit none
    integer :: JA(10)
    integer :: M
    JA = 9
    M = 10
    call ilu0(M,JA)
end program
