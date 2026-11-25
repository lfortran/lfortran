module use_08_mod
    implicit none
    integer, save :: a = 1
end module

module use_08_procs
    implicit none
contains
    subroutine empty_only_shadows()
        ! An empty ONLY list imports nothing (F2018 19.5.2), so a local `a`
        ! here is a different entity from use_08_mod's `a`
        use use_08_mod, only:
        integer :: a
        a = 5
        if (a /= 5) error stop 2
    end subroutine

    subroutine wildcard_still_imports()
        ! A USE without ONLY still imports every public entity. This check is
        ! meaningful only because the program's `use use_08_mod, only:` above
        ! imported nothing: the wildcard here establishes that `a` is visible.
        use use_08_mod
        if (a /= 1) error stop 3
    end subroutine
end module

program use_08
    use use_08_procs, only: empty_only_shadows, wildcard_still_imports
    use use_08_mod, only:
    implicit none
    integer :: a
    a = 5
    if (a /= 5) error stop 1
    call empty_only_shadows()
    call wildcard_still_imports()
end program
