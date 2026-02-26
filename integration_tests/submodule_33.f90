module mod_submodule_33
    implicit none
    interface
        module subroutine test_sub(x)
            integer, intent(out) :: x
        end subroutine
    end interface
end module

submodule(mod_submodule_33) sub_33
    implicit none
contains
    module procedure test_sub
        use iso_c_binding, only: c_int
        integer(c_int) :: val
        val = 42
        x = val
    end procedure
end submodule

program test_submodule_33
    use mod_submodule_33
    implicit none
    integer :: res
    call test_sub(res)
    if (res /= 42) error stop
    print *, "ok"
end program
