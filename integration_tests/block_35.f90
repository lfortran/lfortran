program block_35
    implicit none
    integer :: i
    do i = 1, 3
        call s(i)
        block
            integer :: c(2) = [10, 20]
            if (any(c /= [10, 20] + i - 1)) error stop "program BLOCK array reset"
            c = c + 1
        end block
    end do
contains
    subroutine s(k)
        integer, intent(in) :: k
        block
            integer :: a(3) = [1, 2, 3]
            real, save :: r(2) = [1.5, 2.5]
            logical :: l(2) = [.true., .false.]
            if (any(a /= [1, 2, 3] + k - 1)) error stop "BLOCK array reset"
            if (any(r /= [1.5, 2.5] + k - 1)) error stop "BLOCK SAVE array reset"
            if (any(l .neqv. [mod(k, 2) == 1, mod(k, 2) == 0])) error stop "BLOCK logical array reset"
            a = a + 1
            r = r + 1
            l = .not. l
        end block
    end subroutine
end program
