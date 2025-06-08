module inf_mod_1
    interface is_finite
       module procedure is_finite_1
    end interface is_finite
 contains
    subroutine is_finite_1()
    end subroutine is_finite_1
 end module inf_mod_1
 
 module infnan_mod_1
    use inf_mod_1, only : is_finite
 contains
    subroutine is_finite_1(x)
       integer , intent(in) :: x
       print *, x
       if (x /= 1) error stop
    end subroutine is_finite_1
 end module infnan_mod_1
 
 
 program bobyqa_exmp
    use infnan_mod_1
    call is_finite_1(1)
 end program bobyqa_exmp
 