module mymod
    implicit none

    ! 1. Array-to-array at module scope
    integer, save, public :: arr1(3) = [1, 2, 3]
    integer, public :: arr2(3)
    equivalence (arr1, arr2)

    ! 2. Mixed-type equivalence at module scope
    integer, save, public :: i = 1078530011
    real, public :: r
    equivalence (i, r)

    ! 3. Chained equivalence at module scope
    integer, save, public :: x = 10
    integer, public :: y, z
    equivalence (x, y)
    equivalence (y, z)

contains
    ! 4. Equivalence inside a module subroutine
    subroutine test_sub()
        integer :: local_a
        integer :: local_b
        equivalence (local_a, local_b)

        local_a = 5
        if (local_b /= 5) error stop
    end subroutine test_sub

end module mymod

program main
    use mymod
    implicit none

    ! Test array equivalence
    arr2(2) = 42
    if (arr1(2) /= 42) error stop

    ! Test mixed-type equivalence
    ! 1078530011 is approximately 3.1415927 in single precision IEEE 754
    if (abs(r - 3.1415927) > 1e-6) error stop

    ! Test chained equivalence
    z = 20
    if (x /= 20) error stop
    if (y /= 20) error stop

    ! Test subroutine equivalence
    call test_sub()

end program main
