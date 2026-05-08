program file_open_09
! Test that open(newunit=...) returns a valid F2023 12.5.6.13 unit number:
! must be negative, must not be -1, and must not equal INPUT_UNIT,
! OUTPUT_UNIT, or ERROR_UNIT.
use iso_fortran_env
implicit none
integer :: u1, u2

open(newunit=u1, file="file_open_09.tmp", status="replace", form="formatted")
if (u1 >= -1) error stop "newunit >= -1"
if (u1 == ERROR_UNIT) error stop "newunit == ERROR_UNIT"
if (u1 == INPUT_UNIT) error stop "newunit == INPUT_UNIT"
if (u1 == OUTPUT_UNIT) error stop "newunit == OUTPUT_UNIT"

! A second concurrent open must produce a different unit number.
open(newunit=u2, status="scratch", form="formatted")
if (u2 >= -1) error stop "second newunit >= -1"
if (u2 == u1) error stop "second newunit collides with first"

close(u2)
close(u1, status="delete")
print *, "PASS"
end program
