module interface_14_module
    interface sub
        module procedure sub
    end interface
contains
    subroutine sub()
        print *, "Hello World!"
    end subroutine
end module

program interface_14
    use interface_14_module, only: sub
    implicit none
    call sub()
end program interface_14
