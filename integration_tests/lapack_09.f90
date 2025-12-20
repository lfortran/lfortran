! MRE: COMMON block scalar passed to external subroutine argument.
! Reduced from LAPACK BLAS testing sources with COMMON blocks.
program lapack_09
    implicit none

    integer :: n
    common /combla/ n
    external :: bar

    n = 41
    call bar(n)

    if (n /= 42) error stop
    print *, 'PASS'
end program

subroutine bar(n)
    implicit none

    integer, intent(inout) :: n
    n = n + 1
end subroutine
