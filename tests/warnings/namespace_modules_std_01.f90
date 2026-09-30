! With --std=f23, a namespace import (`use, namespace`) is reported as an
! LFortran extension, in a module, a program, a procedure and a BLOCK.
module namespace_modules_std_01_m
    implicit none
    integer :: x = 1
end module

module namespace_modules_std_01_b
    use, namespace :: m => namespace_modules_std_01_m
    implicit none
end module

program namespace_modules_std_01
    use, namespace :: namespace_modules_std_01_m
    implicit none
    print *, namespace_modules_std_01_m%x
    call s()
    block
        use, intrinsic, namespace :: env => iso_fortran_env
        print *, env%int32
    end block
contains
    subroutine s()
        use, namespace :: m => namespace_modules_std_01_m
        print *, m%x
    end subroutine
end program
