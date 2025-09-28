! Test for private interface redefinition bug
! This reproduces the exact LFortran interface resolution issue found in FPM v0.12.0
!
! EXACT FPM PATTERN - TWO SEPARATE MODULES:
! - First interface in fpm_strings module (lines 83-85)
! - Second interface in M_CLI2 module (lines 2074-2077)
! - No ambiguity because interfaces are in different modules
! ✅ gfortran: compiles successfully (matches real FPM behavior)
! ❌ LFortran: "semantic error: Function 'str' not found"

! Module 1: Like fpm_strings module
module fpm_strings_mod
    use iso_fortran_env, only: int64
    implicit none

    private
    public :: str  ! Make str interface public

    interface str
        module procedure str_int, str_int64, str_logical
    end interface

contains

    pure function str_int(i) result(s)
        integer, intent(in) :: i
        character(len=12) :: s
        write(s, '(i0)') i
    end function

    pure function str_int64(i) result(s)
        integer(int64), intent(in) :: i
        character(len=21) :: s
        write(s, '(i0)') i
    end function

    pure function str_logical(l) result(s)
        logical, intent(in) :: l
        character(len=7) :: s
        if (l) then
            s = ".true."
        else
            s = ".false."
        end if
    end function

end module fpm_strings_mod

! Module 2: Like M_CLI2 module with its own str interface (NO IMPORT!)
module m_cli2_mod
    ! NOTE: M_CLI2 does NOT import fpm_strings - interfaces are completely separate!
    implicit none

    ! M_CLI2 defines its own str interface - no conflict with fpm_strings
    interface str
        module procedure msg_scalar, msg_one
    end interface str

contains

    function msg_scalar(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep) result(s)
        class(*), intent(in), optional :: g0, g1, g2, g3, g4
        class(*), intent(in), optional :: g5, g6, g7, g8, g9
        class(*), intent(in), optional :: ga, gb, gc, gd, ge
        class(*), intent(in), optional :: gf, gg, gh, gi, gj
        character(len=*), intent(in), optional :: sep
        character(len=:), allocatable :: s
        s = "msg_scalar_result"
    end function

    function msg_one(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, sep) result(s)
        class(*), intent(in) :: g0(:)
        class(*), intent(in), optional :: g1(:), g2(:)
        class(*), intent(in), optional :: g3(:), g4(:), g5(:)
        class(*), intent(in), optional :: g6(:), g7(:), g8(:), g9(:)
        character(len=*), intent(in), optional :: sep
        character(len=:), allocatable :: s
        s = "msg_one_result"
    end function

    subroutine journal(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
        character(len=*), intent(in) :: where
        class(*), intent(in) :: g0
        class(*), intent(in), optional :: g1, g2, g3, g4, g5, g6, g7, g8, g9
        class(*), intent(in), optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
        character(len=*), intent(in), optional :: sep

        ! This should use the local M_CLI2 str interface (msg_scalar/msg_one)
        ! The imported fpm_strings str interface is made private by "private str"
        write(*,'(a)') str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
    end subroutine journal

end module m_cli2_mod

program interface_20
    use m_cli2_mod
    implicit none

    call journal("test", 1, 2, 3, 4, 5, 6, 7, 8, 9)
end program