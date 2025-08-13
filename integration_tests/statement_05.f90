subroutine st_fn_bug()
    implicit none
    complex :: ZA, Q, res

    ZA(Q) = cmplx(abs(real(Q)), abs(aimag(Q)))

    res = ZA(complex(1, 2))

    print *, res
    if (res /= complex(1, 2)) error stop
end subroutine st_fn_bug

program statement_05
    implicit none

    call st_fn_bug()
end program