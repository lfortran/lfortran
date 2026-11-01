module modules_89_mod_a
    implicit none
    integer, parameter :: n = 3
end module modules_89_mod_a

module modules_89_mod_b
    use modules_89_mod_a, only: n
    implicit none
contains
    subroutine s(x)
        integer, intent(out) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = i
        end do
    end subroutine s
end module modules_89_mod_b
