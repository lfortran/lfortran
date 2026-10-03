! A parallel do calls procedures contained in the program, which are copied
! out of the program with the region. Symbols of a scope nested in a copied
! procedure (a BLOCK, or the interface of its own parallel region) stay in
! that scope, also when the procedure has a symbol of the same name.
program implicit_interface_106
    implicit none
    integer :: i, b(4), c(4)
    !$omp parallel do
    do i = 1, 4
        b(i) = shadowed(i)
    end do
    !$omp end parallel do
    if (b(1) /= 12 .or. b(2) /= 24 .or. b(3) /= 36 .or. b(4) /= 48) error stop 1
    !$omp parallel do
    do i = 1, 4
        call nested_region(i, c(i))
    end do
    !$omp end parallel do
    if (c(1) /= 10 .or. c(2) /= 20 .or. c(3) /= 30 .or. c(4) /= 40) error stop 2
    print *, b, c
contains
    integer function shadowed(k)
        integer, intent(in) :: k
        integer :: s
        s = 2*k
        block
            integer :: s
            s = 10*k
            shadowed = s
        end block
        shadowed = shadowed + s
    end function

    subroutine nested_region(k, r)
        integer, intent(in) :: k
        integer, intent(out) :: r
        integer :: j, s
        s = 0
        !$omp parallel do reduction(+:s)
        do j = 1, 10
            s = s + k
        end do
        !$omp end parallel do
        r = s
    end subroutine
end program
