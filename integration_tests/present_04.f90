module present_04_mod
    implicit none

    integer, save :: called = 0

contains

    subroutine s(f)
        implicit none

        external :: f
        optional :: f

        if (present(f)) call f()
    end subroutine s
end module present_04_mod

program present_04
    use present_04_mod, only: called, s
    implicit none

    called = 0

    call s()
    if (called /= 0) error stop

    call s(f)
    if (called /= 1) error stop
end program present_04

subroutine f()
    use present_04_mod, only: called
    implicit none

    called = called + 1
end subroutine f
