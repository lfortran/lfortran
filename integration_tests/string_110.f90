! Test character array section substring: s1(:)(1:4)
program string_110
    implicit none
    character(5) :: s1(2)
    character(4) :: s2(2)

    s1 = ['hello', 'world']
    s2 = ['hell', 'worl']

    ! s1(:)(1:4) should produce a character(4) array
    call check(s1(:)(1:4), s2)

    ! Also test with explicit bounds
    call check(s1(1:2)(1:4), s2)

    ! Test with substring starting at offset > 1
    call check_mid(s1(:)(2:4), ['ell', 'orl'])

contains

    subroutine check(a, b)
        character(*), intent(in) :: a(:), b(:)
        integer :: i
        if (len(a) /= len(b)) error stop
        if (size(a) /= size(b)) error stop
        do i = 1, size(a)
            if (a(i) /= b(i)) error stop
        end do
    end subroutine

    subroutine check_mid(a, b)
        character(*), intent(in) :: a(:), b(:)
        integer :: i
        if (len(a) /= len(b)) error stop
        if (size(a) /= size(b)) error stop
        do i = 1, size(a)
            if (a(i) /= b(i)) error stop
        end do
    end subroutine

end program
