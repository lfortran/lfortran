program string_115
implicit none
character(len=10) :: s
s = 'hello'
call test_len_trim_dim(s)
call test_len_trim_dim_const()
contains
    subroutine test_len_trim_dim(s)
        character(*), intent(in) :: s
        character :: a(len(trim(s)))
        a = 'x'
        if (size(a) /= 5) error stop
        print *, "size(a) =", size(a)
    end subroutine

    subroutine test_len_trim_dim_const()
        character :: b(len(trim('world   ')))
        b = 'y'
        if (size(b) /= 5) error stop
        print *, "size(b) =", size(b)
    end subroutine
end program
