! F2023 8.5.16: "A variable, common block, or procedure pointer declared in the
! scoping unit of a main program, a module, or a submodule implicitly has the
! SAVE attribute, which may be confirmed by explicit specification."
!
! Each scope below declares a pair of variables that differ only in whether the
! attribute is written down, so the two must end up identical. None of them is
! initialized in its declaration: an initializer confers the attribute on its
! own, which would hide the rule under test.
!
! Running this cannot tell the two spellings apart -- a main program is entered
! once, so persistence is not observable, and the same values come out either
! way. What this file guards is its ASR reference, registered in
! `tests/tests.toml`: there `implied`, `sub_implied` and `prog_implied` carry
! `Save` without `save` being written, the same storage their written
! counterparts carry. Running it is still worth doing, as a check that
! conferring the attribute does not change what is generated for the
! declarations that already had it.
module implicit_save_01_mod
    implicit none
    integer :: implied
    integer, save :: written

    interface
        module subroutine sub_reset()
        end subroutine sub_reset

        module subroutine sub_bump(i, w)
            integer, intent(out) :: i, w
        end subroutine sub_bump
    end interface

contains

    subroutine bump()
        implied = implied + 1
        written = written + 1
    end subroutine bump

end module implicit_save_01_mod

submodule (implicit_save_01_mod) implicit_save_01_submod
    implicit none
    integer :: sub_implied
    integer, save :: sub_written

contains

    module subroutine sub_reset()
        sub_implied = 0
        sub_written = 0
    end subroutine sub_reset

    module subroutine sub_bump(i, w)
        integer, intent(out) :: i, w
        sub_implied = sub_implied + 1
        sub_written = sub_written + 1
        i = sub_implied
        w = sub_written
    end subroutine sub_bump

end submodule implicit_save_01_submod

program implicit_save_01
    use implicit_save_01_mod
    implicit none
    integer :: prog_implied
    integer, save :: prog_written
    integer :: si, sw

    implied = 0
    written = 0
    call bump()
    call bump()
    call bump()
    if (implied /= 3) error stop
    if (written /= 3) error stop

    call sub_reset()
    call sub_bump(si, sw)
    call sub_bump(si, sw)
    if (si /= 2) error stop
    if (sw /= 2) error stop

    prog_implied = implied
    prog_written = written
    if (prog_implied /= 3) error stop
    if (prog_written /= 3) error stop

    print *, "ok"
end program implicit_save_01
