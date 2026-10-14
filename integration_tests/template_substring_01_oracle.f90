! Concrete standard-Fortran oracle for template_substring_01, with the
! deferred constant replaced by the instantiation's value.
module template_substring_01_oracle_m
    implicit none
contains
    subroutine f4(t)
        integer, parameter :: n = 4
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

    subroutine g3()
        integer, parameter :: n = 3
        print *, 'xyzw'(1:n), 'xyzw'(n:), 'xyzw'(:n-1)
        if ('xyzw'(1:n) /= 'xyz') error stop
        if (len('xyzw'(1:n)) /= n) error stop
        if ('xyzw'(n:) /= 'zw') error stop
        if ('xyzw'(:n-1) /= 'xy') error stop
        if ('xyzw'(n:n) /= 'z') error stop
    end subroutine

    subroutine h4(a)
        integer, parameter :: n = 4
        character(len=8), intent(in) :: a(:)
        character(len=n) :: b(size(a))
        b = a(:)(1:n)
        print *, b, a(:)(n:n)
        if (any(b /= ['abcd', 'ijkl'])) error stop
        if (any(a(:)(n:n) /= ['d', 'l'])) error stop
        if (len(a(:)(2:n)) /= n - 1) error stop
    end subroutine
end module

program template_substring_01_oracle
    use template_substring_01_oracle_m
    implicit none
    call f4('abcd')
    call g3()
    call h4(['abcdefgh', 'ijklmnop'])
end program
