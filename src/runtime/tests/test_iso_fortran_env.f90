program test_iso_fortran_env
use lfortran_intrinsic_iso_fortran_env, only: &
    int32, int64, real32, real64, lock_type, event_type, team_type
implicit none
real(real32) :: r4
real(real64) :: r8
integer(int32) :: i4
integer(int64) :: i8
type(lock_type) :: lock
type(event_type) :: event
type(team_type) :: team

if (kind(r4) /= 4) error stop
if (kind(r8) /= 8) error stop
if (kind(i4) /= 4) error stop
if (kind(i8) /= 8) error stop

lock = lock_type()
event = event_type()
team = team_type()
end program
