! Under --detect-leaks, the startup record of a module carries the teardown
! that frees the module's storage, and the leak report runs the teardown of
! every record that was initialized before it counts. The runtime keeps as
! many as get initialized: module_teardown_01c.c publishes far more records
! than any fixed-size list would hold, the way JIT code publishes its own,
! and checks at exit that the report ran every one of their teardowns.
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
