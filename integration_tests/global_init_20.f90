! global_init_20_a and global_init_20_b from a static archive, linked with
! dead stripping into a program that does not use either module: only the
! archive members the program's calls pull in are linked, and those must
! still be initialized before the first call; see CMakeLists.txt.
program global_init_20
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function global_init_20_check_initial() bind(c)
            import :: c_int
        end function global_init_20_check_initial
        subroutine global_init_20_mutate() bind(c)
        end subroutine global_init_20_mutate
        integer(c_int) function global_init_20_check_mutated() bind(c)
            import :: c_int
        end function global_init_20_check_mutated
    end interface
    integer(c_int) :: rc
    rc = global_init_20_check_initial()
    if (rc /= 0) then
        print *, "initial check", rc
        error stop 1
    end if
    call global_init_20_mutate()
    rc = global_init_20_check_mutated()
    if (rc /= 0) then
        print *, "mutated check", rc
        error stop 2
    end if
    print *, "ok"
end program global_init_20
