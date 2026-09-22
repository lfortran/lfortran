! Sections passed to a device function inside a SELECT CASE in the
! offloaded loop. Each section is gathered inside the case that uses it: a
! section valid only in its own case, a section in the selector, and a
! section whose extent reads the index of a loop nested in a case. The host
! value of that index would overrun the gathered buffer if the gather ran
! outside the loop.
module gpu_metal_352_mod
implicit none
contains

pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function

end module

program gpu_metal_352
use gpu_metal_352_mod
implicit none
real, allocatable :: a(:,:)
real :: s(3,3), v(3), w(3)
integer :: i, j, k, l
allocate(a(3,5))
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do

! Sections valid only in their own case.
do concurrent (i = 1:3)
    select case (i)
    case (1)
        v(i) = row_sum(a(i,1:2*i))
    case default
        v(i) = row_sum(a(i,2*i-3:2*i-2))
    end select
end do
print *, v
if (v(1) /= sum(a(1,1:2))) error stop
do k = 2, 3
    if (v(k) /= sum(a(k,2*k-3:2*k-2))) error stop
end do

! A section in the selector, and sections in the cases it selects.
do concurrent (i = 1:3)
    select case (int(row_sum(a(i,1:i))))
    case (:20)
        w(i) = row_sum(a(i,i:i+1))
    case (21:60)
        w(i) = -row_sum(a(i,1:2*i))
    case default
        w(i) = 0
    end select
end do
print *, w
if (w(1) /= sum(a(1,1:2))) error stop
if (w(2) /= -sum(a(2,1:4))) error stop
if (w(3) /= 0) error stop

! An extent that reads the index of a do concurrent nested in a case.
s = 0
j = 1000
do concurrent (i = 1:3)
    select case (i)
    case (1:2)
        do concurrent (j = 1:3)
            s(i,j) = row_sum(a(i,1:j))
        end do
    case default
        s(i,1) = -1
    end select
end do
print *, s
do k = 1, 2
    do l = 1, 3
        if (s(k,l) /= sum(a(k,1:l))) error stop
    end do
end do
if (s(3,1) /= -1) error stop
if (s(3,2) /= 0 .or. s(3,3) /= 0) error stop

! The same with an inner do and a select case nested in it.
s = 0
do concurrent (i = 1:3)
    select case (i)
    case (1, 3)
        do j = 1, 3
            select case (j)
            case (2)
                s(i,j) = row_sum(a(i,j:j+3))
            case default
                if (j < 3) s(i,j) = row_sum(a(i,1:j))
            end select
        end do
    case default
        s(i,1) = row_sum(a(i,4:5))
    end select
end do
print *, s
do k = 1, 3, 2
    if (s(k,1) /= a(k,1)) error stop
    if (s(k,2) /= sum(a(k,2:5))) error stop
    if (s(k,3) /= 0) error stop
end do
if (s(2,1) /= sum(a(2,4:5))) error stop
if (s(2,2) /= 0 .or. s(2,3) /= 0) error stop
end program
