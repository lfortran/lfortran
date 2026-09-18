! `template`, `requirement`, `require`, `instantiate` and `deferred` are the
! keywords of the experimental templates prototype. They are not reserved
! words in Fortran, so they must keep working as ordinary identifiers whether
! or not `--enable-experimental-feature templates` is passed.
module reserved_04_m
implicit none

type :: deferred
    integer :: require
    integer :: instantiate
end type

contains

integer function template(requirement)
integer, intent(in) :: requirement
template = requirement + 1
end function

end module

program reserved_04
use reserved_04_m
implicit none
type(deferred) :: instantiate
integer :: require, requirement

require = 3
requirement = template(require)
instantiate%require = 5
instantiate%instantiate = instantiate%require + 1

if (requirement /= 4) error stop
if (template(requirement) /= 5) error stop
if (instantiate%require /= 5) error stop
if (instantiate%instantiate /= 6) error stop
end program
