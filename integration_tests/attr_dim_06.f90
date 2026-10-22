! A DIMENSION statement followed by a type declaration without an array
! spec is valid; only a second array spec is a duplicate DIMENSION
! attribute (tests/errors/continue_compilation_1.f90 covers that).
subroutine fill(x, n)
    integer :: n
    dimension x(*)
    real, intent(out) :: x
    integer :: i
    do i = 1, n
        x(i) = 2.0 * i
    end do
end subroutine

program attr_dim_06
    dimension b(4), c(2)
    real, target :: b
    character(len=3) :: c
    real, pointer :: p(:)
    call fill(b, 4)
    p => b
    if (abs(p(4) - 8.0) > 1e-6) error stop
    c = 'abc'
    c(2) = 'xyz'
    if (c(1) /= 'abc' .or. c(2) /= 'xyz') error stop
    print *, b, c
end program
