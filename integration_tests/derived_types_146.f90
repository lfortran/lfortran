module derived_types_146_mod
    implicit none

    type :: workflow
        integer :: dummy
    end type

    type :: compiler
        integer :: id
        character(20) :: name
    end type

    interface compiler
        module procedure compiler_new
    end interface

contains

    function compiler_new() result(step)
        type(workflow) :: step
    end function

    subroutine build_compilerinfo()
        type(compiler) :: info

        select case (1)
        case (1)
            info = compiler(1, 'UNKNOWN')
        end select
        
        if (info%id /= 1) error stop
        if (trim(info%name) /= 'UNKNOWN') error stop
    end subroutine
end module

program derived_types_146
    use derived_types_146_mod
    implicit none
    call build_compilerinfo()
    print *, "OK"
end program
