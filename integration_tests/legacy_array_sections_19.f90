module legacy_array_sections_19_m1
    implicit none
contains
    subroutine update(c, n)
        integer, intent(in) :: n
        real, intent(inout) :: c(n, *)
        c(1, 1) = c(1, 1) + 10.0
        c(2, 1) = c(2, 1) + 20.0
    end subroutine
end module

module legacy_array_sections_19_m2
    use legacy_array_sections_19_m1
    implicit none
contains
    subroutine work(a, n)
        integer, intent(in) :: n
        real, intent(inout) :: a(n, *)
        call update(a(2, 2), n)
    end subroutine
end module

program legacy_array_sections_19
    use legacy_array_sections_19_m2
    implicit none
    real :: A(3,3)
    A = 0.0
    call work(A, 3)
    if (A(2,2) /= 10.0) error stop
    if (A(3,2) /= 20.0) error stop
    print *, "PASS"
end program
