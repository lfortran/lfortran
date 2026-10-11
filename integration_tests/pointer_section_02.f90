program pointer_section_02
    implicit none
    integer :: a(24)
    call sub(a, 24)
contains
    subroutine sub(x, n)
        integer, intent(in) :: n
        integer, target, intent(out) :: x(n)
        integer, pointer :: p(:, :)
        x = 0
        p(1:4, 1:6) => x(1:n)
        p(4, 6) = 7
        if (p(4, 6) /= 7 .and. p(1, 1) /= 0) error stop
    end subroutine
end program pointer_section_02
