! An impure elemental procedure may have side effects; an elemental one
! without the IMPURE prefix is pure (Fortran 2018 15.8.1).
module elemental_26_mod
    implicit none
    integer :: counter = 0
contains
    impure elemental integer function bump(n)
        integer, intent(in) :: n
        counter = counter + 1
        print *, "bump", n
        bump = n + counter
    end function bump

    impure elemental subroutine accumulate(x)
        integer, intent(inout) :: x
        counter = counter + x
        x = counter
    end subroutine accumulate

    elemental integer function twice(n)
        integer, intent(in) :: n
        twice = 2*n
    end function twice
end module elemental_26_mod

program elemental_26
    use elemental_26_mod, only: counter, bump, accumulate, twice
    implicit none
    integer :: a(3), b(3)
    a = bump([1, 2, 3])
    print *, a, counter
    if (any(a /= [2, 4, 6])) error stop
    if (counter /= 3) error stop
    b = [1, 2, 3]
    call accumulate(b)
    print *, b, counter
    if (any(b /= [4, 6, 9])) error stop
    if (counter /= 9) error stop
    if (any(twice(b) /= [8, 12, 18])) error stop
end program elemental_26
