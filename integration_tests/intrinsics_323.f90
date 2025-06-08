program intrinsics_323
    real, allocatable :: L(:,:), U(:,:)
    call lu(L, U)
contains
subroutine lu(L, U)
    real, intent(out), allocatable :: L(:,:), U(:,:)

    allocate(L(5,5), U(5,5))
    L = 12.91
    U = -12.91
    print *, dot_product(L(4,1:3), U(1:3,4))
    if (abs(dot_product(L(4,1:3), U(1:3,4)) - (-500.004272)) > 1e-8) error stop
end subroutine
end program
