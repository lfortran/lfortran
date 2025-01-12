module m_continue_compilation_5

    
    public

    type tt
    end type tt

contains

    subroutine test_sub()

        class(tt) :: local_var

    end subroutine test_sub

end module 


! https://github.com/lfortran/lfortran/issues/5872
program continue_compilation_5

    use m_continue_compilation_5
    ! Class declaration must be dummy, allocatable, or pointer


    
end program continue_compilation_5
