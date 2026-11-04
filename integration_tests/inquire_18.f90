! INQUIRE without OPEN: file= and unit= inquiries on a C/C++ backend
! (visit_FileInquire). Uses names guaranteed absent so no file setup
! with OPEN is needed.
program inquire_18
implicit none
logical :: ex, op
integer :: ios
inquire(file="lfortran_inquire_18_absent.tmp", exist=ex, opened=op, iostat=ios)
if (ex) error stop 1
if (op) error stop 2
if (ios /= 0) error stop 3
inquire(unit=10, opened=op, exist=ex, iostat=ios)
if (op) error stop 4
if (ios /= 0) error stop 5
print *, ex, op, ios
end program inquire_18
