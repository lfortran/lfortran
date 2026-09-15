! A statement function that references a procedure with an implicit interface,
! in a main program that also references the procedure directly: before,
! during and after a DO WHILE loop, in a BLOCK, and in an internal procedure
! with a statement function of its own.
program implicit_interface_87
    implicit none
    real, external :: ii87_g
    real :: sf, y, s
    integer :: it
    sf(y) = ii87_g(y) - 1.0
    it = 0
    if (ii87_g(real(it)) < 5.0) it = 7
    if (it /= 7) error stop 1
    it = 0
    do while (ii87_g(real(it)) < 3.0)
        it = it + 1
    end do
    if (it /= 3) error stop 2
    s = sf(3.0)
    if (abs(s - 2.0) > 1e-6) error stop 3
    block
        real :: b
        b = ii87_g(4.0) + sf(2.0)
        if (abs(b - 5.0) > 1e-6) error stop 4
    end block
    call inner(s)
    if (abs(s - 4.0) > 1e-6) error stop 5
    print *, it, s
contains
    subroutine inner(z)
        real, intent(inout) :: z
        real :: tf, w
        tf(w) = ii87_g(w) + 1.0
        z = z + ii87_g(1.0) + tf(0.0)
    end subroutine
end program

real function ii87_g(x)
    real, intent(in) :: x
    ii87_g = x
end function
