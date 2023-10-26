SUBROUTINE dinvr(x)
IMPLICIT NONE
DOUBLE PRECISION x
DOUBLE PRECISION big,small,zx,zy,zz, zsmall, zbig
LOGICAL qcond
INTRINSIC abs,max,min
LOGICAL qxmon
SAVE

qxmon(zx,zy,zz) = zx .LE. zy .AND. zy .LE. zz

qcond = .NOT. qxmon(small,x,big)

print *, "small = ", small
print *, "x = ", x
print *, "big = ", big

print *, 'qcond = ', qcond
IF (qcond) error stop

entry distinv(zsmall, zbig)

small = -1.6d0
big = 1.9d0

return


end subroutine

program entry_10
implicit none
double precision :: x
x = 0.5d0

call distinv(0.0d0, 1.0d0)

call dinvr(x)
end program
