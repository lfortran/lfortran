program block_35
    implicit none
    integer :: i
    do i = 1, 3
        call s(i)
        block
            integer :: c(2) = [10, 20]
            complex(8) :: zc(2) = [(1.0d0, 2.0d0), (3.0d0, 4.0d0)]
            if (any(c /= [10, 20] + i - 1)) error stop "program BLOCK array reset"
            if (any(real(zc) /= [1, 3] + i - 1)) error stop "program BLOCK complex array reset"
            c = c + 1
            zc = zc + 1
        end block
    end do
contains
    subroutine s(k)
        integer, intent(in) :: k
        block
            integer :: a(3) = [1, 2, 3]
            real, save :: r(2) = [1.5, 2.5]
            logical :: l(2) = [.true., .false.]
            complex :: z = (7.0, 8.0), zs(2) = [(1.0, -1.0), (2.0, -2.0)]
            if (any(a /= [1, 2, 3] + k - 1)) error stop "BLOCK array reset"
            if (any(r /= [1.5, 2.5] + k - 1)) error stop "BLOCK SAVE array reset"
            if (any(l .neqv. [mod(k, 2) == 1, mod(k, 2) == 0])) error stop "BLOCK logical array reset"
            if (z /= cmplx(6 + k, 8)) error stop "BLOCK complex reset"
            if (any(zs /= [cmplx(k, -1), cmplx(1 + k, -2)])) error stop "BLOCK complex array reset"
            a = a + 1
            r = r + 1
            l = .not. l
            z = z + 1
            zs = zs + 1
        end block
    end subroutine
end program
