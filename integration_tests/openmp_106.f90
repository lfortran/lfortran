! A call in a parallel region through a procedure pointer whose interface
! has an explicit-shape dummy with a use-associated extent (#14512)
module openmp_106_sizes
    implicit none
    integer :: n = 2
end module

module openmp_106_m
    implicit none
    abstract interface
        subroutine cb(a)
            use openmp_106_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine fa(a)
        use openmp_106_sizes, only: n
        real :: a(n)
        a = 2.0
    end subroutine
end module

program openmp_106
    use openmp_106_m
    implicit none
    procedure(cb), pointer :: p
    real :: z(2)
    p => fa
    z = 0.0
!$omp parallel num_threads(1)
    call p(z)
!$omp end parallel
    print *, z
    if (z(1) /= 2.0 .or. z(2) /= 2.0) error stop
end program
