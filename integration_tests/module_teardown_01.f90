! Under --detect-leaks, the object file that defines a module registers the
! teardown that frees the module's storage with the runtime, and the leak
! report runs every registered teardown before it counts. The runtime keeps
! as many as get registered: module_teardown_01c.c registers far more than
! any fixed-size list would hold, each through the same entry point a module
! uses, and checks at exit that the report ran every one of them.
program module_teardown_01
    use iso_c_binding, only: c_int
    implicit none
    interface
        subroutine register_teardowns() bind(c)
        end subroutine register_teardowns
        integer(c_int) function teardowns_run() bind(c)
            import :: c_int
        end function teardowns_run
    end interface
    call register_teardowns()
    ! The report at the end of the program runs them, and nothing before it.
    if (teardowns_run() /= 0) error stop 1
    print *, "ok"
end program module_teardown_01
