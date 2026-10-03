! Regression test: an entity named in a DIMENSION statement before its
! type declaration must get the lower-cased symbol name every other
! declaration path produces. Spelled in upper case, the symbol was
! created as `ARRAY` while the scope registered it as `array`, and the
! translation unit failed ASR verification (attr_dim_01 is the same
! shape in lower case).
subroutine SUB(ARRAY, N)
    integer :: N
    dimension ARRAY(N)
    double precision ARRAY
    integer :: I
    do I = 1, N
        ARRAY(I) = 1.5d0 * I
    end do
end subroutine

program attr_dim_05
    double precision :: arr(3)
    call SUB(arr, 3)
    if (abs(arr(1) - 1.5d0) > 1d-12) error stop
    if (abs(arr(3) - 4.5d0) > 1d-12) error stop
end program
