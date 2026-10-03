! A module whose pointers are initially associated with parts of another
! module's variable, both reached only through a separately compiled
! external subroutine; see run_show_llvm_test.cmake.
program test_init_show_llvm
    implicit none
    interface
        subroutine test_init_show_llvm_s()
        end subroutine test_init_show_llvm_s
    end interface
    call test_init_show_llvm_s()
end program test_init_show_llvm
