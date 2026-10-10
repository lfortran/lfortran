! A namelist group object declared after the NAMELIST statement is typed by the
! implicit typing rules there; a later type declaration must confirm that type.
module namelist_implicit_type_01_mod
    type :: pt
        integer :: a
    end type
end module

subroutine namelist_confirm_integer_as_real
    namelist /g1/ n
    real :: n  ! {Error} namelist object 'n' was implicitly typed integer(4) at the namelist statement; its declaration as real(4) does not confirm that type
end subroutine

subroutine namelist_confirm_real_as_derived_type
    use namelist_implicit_type_01_mod, only: pt
    namelist /g2/ tv
    type(pt) :: tv  ! {Error} namelist object 'tv' was implicitly typed real(4) at the namelist statement; its declaration as type(pt) does not confirm that type
end subroutine

subroutine namelist_confirm_real_as_character
    namelist /g3/ c
    character(len=5) :: c  ! {Error} namelist object 'c' was implicitly typed real(4) at the namelist statement; its declaration as character(len=5) does not confirm that type
end subroutine

subroutine namelist_confirm_real_kind
    namelist /g4/ d
    real(8) :: d  ! {Error} namelist object 'd' was implicitly typed real(4) at the namelist statement; its declaration as real(8) does not confirm that type
end subroutine

subroutine namelist_confirm_ok
    namelist /g5/ x, y, k
    real :: x
    real :: y(3)
    integer :: k
end subroutine
