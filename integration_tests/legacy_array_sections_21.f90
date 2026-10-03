module las21_m1
    implicit none
contains
    subroutine set_val(a, lda, val)
        integer, intent(in) :: lda
        real, intent(inout) :: a(lda, *)
        real, intent(in) :: val
        a(1, 1) = val
        a(1, 2) = val
    end subroutine
end module

module las21_m2
    use las21_m1
    implicit none
contains
    subroutine caller(a, lda)
        integer, intent(in) :: lda
        real, intent(inout) :: a(lda, *)
        call set_val(a(2, 2), lda, 99.0)
    end subroutine
end module

program legacy_array_sections_21
    use las21_m2
    implicit none
    real :: A(3,3)
    A = 0.0
    call caller(A, 3)
    print *, A(2,2)
    print *, A(2,3)
    if (A(2,2) /= 99.0) error stop
    if (A(2,3) /= 99.0) error stop
end program
