! Compiled before the submodules, so it cannot know their variables, which
! the submodules' own object files must set up (#14167).
program submodule_60
    use submodule_60_m
    implicit none
    integer :: code
    ! The nested submodule is entered first.
    code = s2_check()
    if (code /= 0) then
        print *, "s2_check:", code
        error stop 1
    end if
    code = s_check(.false.)
    if (code /= 0) then
        print *, "s_check:", code
        error stop 2
    end if
    call s_fill()
    code = s_check(.true.)
    if (code /= 0) then
        print *, "s_check after s_fill:", code
        error stop 3
    end if
    print *, "ok"
end program
