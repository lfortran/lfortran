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

! Keyword arguments in a non lowercase structure constructor, including the
! out of order form where the keywords do not follow component declaration
! order, and the mixed positional/keyword form.
type(date), parameter :: kw_upper = DATE(year=2020, day=9)
type(date), parameter :: kw_mixed = Date(10, year=2021)
type(date) :: kw_var = DATE(year=2022, day=11)

! A nested structure constructor: a component of `single_score` is itself a
! derived type, so the inner constructor has to be folded recursively before
! the outer one can become a compile time constant.
type(single_score), parameter :: nested_upper = SINGLE_SCORE(DATE(7, 2019), 42)
type(single_score), parameter :: nested_arr(2) = &
    [SINGLE_SCORE(DATE(5, 2017), 1), Single_Score(Date(4, 2016), 2)]

! The derived type `date_t` is defined in derived_types_176_m and imported here,
! so a constructor spelled for it in this program unit resolves to an
! ExternalSymbol that has to be unwrapped after the (now case insensitive)
! lookup.  Same three shapes as above, for the imported type.
type(date_t), parameter :: ext_const = DATE_T(27, 2015)
type(date_t), parameter :: ext_arr(2) = [DATE_T(28, 2016), Date_T(29, 2017)]
type(date_t) :: ext_var = DATE_T(30, 2018)

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

! The imported derived type, constructed here with a non lowercase spelling.
if (ext_const%year /= 2015) error stop 32
if (ext_const%day /= 27) error stop 33
if (ext_arr(1)%year /= 2016) error stop 34
if (ext_arr(1)%day /= 28) error stop 35
if (ext_arr(2)%year /= 2017) error stop 36
if (ext_arr(2)%day /= 29) error stop 37
if (ext_var%year /= 2018) error stop 38
if (ext_var%day /= 30) error stop 39

! Keyword arguments, out of order and mixed with a positional argument.
if (kw_upper%year /= 2020) error stop 40
if (kw_upper%day /= 9) error stop 41
if (kw_mixed%year /= 2021) error stop 42
if (kw_mixed%day /= 10) error stop 43
if (kw_var%year /= 2022) error stop 44
if (kw_var%day /= 11) error stop 45

! Nested structure constructors, scalar and array.
if (nested_upper%play_day%year /= 2019) error stop 46
if (nested_upper%play_day%day /= 7) error stop 47
if (nested_upper%score /= 42) error stop 48
if (nested_arr(1)%play_day%year /= 2017) error stop 49
if (nested_arr(1)%play_day%day /= 5) error stop 50
if (nested_arr(1)%score /= 1) error stop 51
if (nested_arr(2)%play_day%year /= 2016) error stop 52
if (nested_arr(2)%play_day%day /= 4) error stop 53
if (nested_arr(2)%score /= 2) error stop 54

call check_from_module()

print *, today%year, arr(1)%year, arr(2)%year, setup%play_day%year
print *, "ok"

end program derived_types_176
