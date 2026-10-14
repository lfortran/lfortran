! Substrings of a dummy argument, of a literal and of an array section whose
! bounds are given by a deferred constant of a template.

module template_substring_01_m
    implicit none

    template tm {n}
        deferred integer, parameter :: n
    contains
        subroutine f(t)
            character(len=n), intent(in) :: t
            print *, t(2:), t(1:n), t(:n), t(n:n)
            if (t(2:) /= 'bcd') error stop
            if (len(t(2:)) /= n - 1) error stop
            if (t(1:n) /= 'abcd') error stop
            if (len(t(:n)) /= n) error stop
            if (t(n:n) /= 'd') error stop
            if (t(1:n-1) /= 'abc') error stop
            if (len(t(n+1:n)) /= 0) error stop
        end subroutine

        subroutine h(a)
            character(len=8), intent(in) :: a(:)
            character(len=n) :: b(size(a))
            b = a(:)(1:n)
            print *, b, a(:)(n:n)
            if (any(b /= ['abcd', 'ijkl'])) error stop
            if (any(a(:)(n:n) /= ['d', 'l'])) error stop
            if (len(a(:)(2:n)) /= n - 1) error stop
        end subroutine

        subroutine g()
            print *, 'xyzw'(1:n), 'xyzw'(n:), 'xyzw'(:n-1)
            if ('xyzw'(1:n) /= 'xyz') error stop
            if (len('xyzw'(1:n)) /= n) error stop
            if ('xyzw'(n:) /= 'zw') error stop
            if ('xyzw'(:n-1) /= 'xy') error stop
            if ('xyzw'(n:n) /= 'z') error stop
        end subroutine
    end template
end module

program template_substring_01
    use template_substring_01_m
    implicit none
    instantiate tm {4}, only: f4 => f
    instantiate tm {3}, only: g3 => g
    instantiate tm {4}, only: h4 => h
    call f4('abcd')
    call g3()
    call h4(['abcdefgh', 'ijklmnop'])
end program
