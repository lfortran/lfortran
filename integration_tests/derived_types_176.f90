program derived_types_176
use derived_types_176_m
implicit none

type :: date
    integer :: day = 1
    integer :: year = 2008
end type date

! A named constant in a main program whose initializer is a structure
! constructor spelled in uppercase.  Before the fix the name was looked up
! without lowering it first, so it did not resolve, the initializer was not
! folded to a compile time constant and code generation emitted a store into
! a constant ("Store operand must be a pointer").
type(date), parameter :: today = DATE(21, 2009)
type(date), parameter :: today_mixed = Date(22, 2010)
type(date), parameter :: today_lower = date(23, 2011)

! An array of the derived type, which reported a bogus
! "Array members must ... be of the same type as the struct" error, because
! the constructor name was compared to the type name case sensitively.
type(date), parameter :: arr(2) = [DATE(24, 2012), Date(25, 2013)]

! A component default that refers to the named constant, i.e. the example in
! Note 2 of "7.5.4.6 Default initialization for components" of F2018, which is
! the code reported in the issue.
type :: single_score
    type(date) :: play_day = TODAY
    integer :: score = 0
end type single_score

type(single_score) :: setup
type(date) :: local_var = DATE(26, 2014)

! The explicitly given component value must override the component default.
if (today%year /= 2009) error stop 1
if (today%day /= 21) error stop 2
if (today_mixed%year /= 2010) error stop 3
if (today_mixed%day /= 22) error stop 4
if (today_lower%year /= 2011) error stop 5
if (today_lower%day /= 23) error stop 6

if (arr(1)%year /= 2012) error stop 7
if (arr(1)%day /= 24) error stop 8
if (arr(2)%year /= 2013) error stop 9
if (arr(2)%day /= 25) error stop 10

if (setup%play_day%year /= 2009) error stop 15
if (setup%play_day%day /= 21) error stop 16
if (setup%score /= 0) error stop 17

if (local_var%year /= 2014) error stop 18
if (local_var%day /= 26) error stop 19

! The same shapes declared in a module.
if (mod_upper%year /= 2009) error stop 20
if (mod_upper%day /= 21) error stop 21
if (mod_mixed%year /= 2010) error stop 22
if (mod_mixed%day /= 22) error stop 23
if (mod_lower%year /= 2011) error stop 24
if (mod_lower%day /= 23) error stop 25
if (mod_arr(1)%year /= 2012) error stop 26
if (mod_arr(1)%day /= 24) error stop 27
if (mod_arr(2)%year /= 2013) error stop 28
if (mod_arr(2)%day /= 25) error stop 29
if (mod_var%year /= 2014) error stop 30
if (mod_var%day /= 26) error stop 31

call check_from_module()

print *, today%year, arr(1)%year, arr(2)%year, setup%play_day%year
print *, "ok"

end program derived_types_176
