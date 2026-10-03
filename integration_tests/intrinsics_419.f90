program intrinsics_419
! Test maxval/minval with dim argument on pointer arrays
implicit none
real, pointer :: work(:)
real :: nrm
integer, pointer :: iwork(:)
integer :: ival

allocate(work(5))
work = [1.0, 5.0, 3.0, 4.0, 2.0]

nrm = maxval(work, 1)
print *, nrm
if (nrm /= 5.0) error stop

nrm = minval(work, 1)
print *, nrm
if (nrm /= 1.0) error stop

allocate(iwork(4))
iwork = [10, 40, 20, 30]

ival = maxval(iwork, 1)
print *, ival
if (ival /= 40) error stop

ival = minval(iwork, 1)
print *, ival
if (ival /= 10) error stop

deallocate(work)
deallocate(iwork)
end program
