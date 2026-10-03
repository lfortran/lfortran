module derived_types_176_m
implicit none

type :: date_t
    integer :: day = 1
    integer :: year = 2008
end type date_t

! A structure constructor in the initializer of a module level named constant,
! written with the same non lowercase spelling as the type declaration.  The
! name has to be resolved case insensitively, otherwise the explicitly given
! component value is silently replaced by the component default.
type(date_t), parameter :: mod_upper = DATE_T(21, 2009)
type(date_t), parameter :: mod_mixed = Date_T(22, 2010)
! The lowercase spelling, which always worked, as a control.
type(date_t), parameter :: mod_lower = date_t(23, 2011)

! The same thing for an array of the derived type.
type(date_t), parameter :: mod_arr(2) = [DATE_T(24, 2012), Date_T(25, 2013)]

! A plain module variable (not a named constant), also initialized statically.
type(date_t) :: mod_var = DATE_T(26, 2014)

! A component default that refers to a named constant built by a non lowercase
! structure constructor, as in Note 2 of F2018 7.5.4.6.
type :: score_t
    type(date_t) :: play_day = mod_upper
    integer :: score = 0
end type score_t

contains

subroutine check_from_module()
    type(score_t) :: setup
    if (mod_upper%year /= 2009) error stop 11
    if (mod_upper%day /= 21) error stop 12
    if (setup%play_day%year /= 2009) error stop 13
    if (setup%play_day%day /= 21) error stop 14
end subroutine check_from_module

end module derived_types_176_m
