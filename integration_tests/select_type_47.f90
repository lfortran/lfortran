program select_type_47
implicit none
call test_re_im((1.0, 2.0))
call test_re_im_dp(dcmplx(3.0d0, 4.0d0))
contains
subroutine test_re_im(a)
    class(*), intent(in) :: a
    select type (a)
    type is (complex(4))
        print *, a%re
        print *, a%im
        if (abs(a%re - 1.0) > 1e-5) error stop 1
        if (abs(a%im - 2.0) > 1e-5) error stop 2
    class default
        error stop 3
    end select
end subroutine

subroutine test_re_im_dp(a)
    class(*), intent(in) :: a
    select type (a)
    type is (complex(8))
        print *, a%re
        print *, a%im
        if (abs(a%re - 3.0d0) > 1d-10) error stop 4
        if (abs(a%im - 4.0d0) > 1d-10) error stop 5
    class default
        error stop 6
    end select
end subroutine
end program
