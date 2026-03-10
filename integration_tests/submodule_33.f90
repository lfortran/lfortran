! Test that a module procedure named "sleep" shadows the intrinsic "sleep"
! when called from a submodule via host association.
module submodule_33_mod
    implicit none
    interface
        module subroutine sleep(millisec)
            integer, intent(in) :: millisec
        end subroutine
        module subroutine test()
        end subroutine
    end interface
end module

submodule (submodule_33_mod) submodule_33_impl
    implicit none
contains
    module subroutine sleep(millisec)
        integer, intent(in) :: millisec
        if (millisec /= 100) error stop
    end subroutine
    module subroutine test()
        call sleep(millisec=100)
    end subroutine
end submodule

program submodule_33_main
    use submodule_33_mod
    implicit none
    call test()
    print *, "ok"
end program
