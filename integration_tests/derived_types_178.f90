! Assigning to a derived-type variable a function result computed from that
! same variable's own allocatable component: t = f(t%v).
! The function result must be computed before the assignment takes place
! (F2018 10.2.1.3), so the component must not be released first.
program derived_types_178
use derived_types_178_m
implicit none

type(tt) :: t
type(tt) :: ta(2)
type(tt), target :: w, z
type(tt), pointer :: ptr1, ptr2
type(ts) :: u
integer :: i

allocate(t%v(3))
t%v = [1.0, 2.0, 3.0]
t = f(t%v)
if (.not. allocated(t%v)) error stop
if (size(t%v) /= 3) error stop
if (any(t%v /= [1.0, 2.0, 3.0])) error stop

! a section of the target's own component
deallocate(t%v)
allocate(t%v(3))
t%v = [1.0, 2.0, 3.0]
t = f(t%v(1:2))
if (size(t%v) /= 2) error stop
if (any(t%v /= [1.0, 2.0])) error stop

! an element of an array of derived type
do i = 1, 2
    allocate(ta(i)%v(2))
    ta(i)%v = [real(i), real(2*i)]
end do
do i = 1, 2
    ta(i) = f(ta(i)%v)
end do
if (size(ta(1)%v) /= 2) error stop
if (any(ta(1)%v /= [1.0, 2.0])) error stop
if (size(ta(2)%v) /= 2) error stop
if (any(ta(2)%v /= [2.0, 4.0])) error stop

! the right hand side must be evaluated exactly once
deallocate(t%v)
allocate(t%v(3))
t%v = [1.0, 2.0, 3.0]
ncalls = 0
t = g(t%v)
if (ncalls /= 1) error stop
if (size(t%v) /= 3) error stop
if (any(t%v /= [2.0, 4.0, 6.0])) error stop

! a substring of the target's own character component
u%s = 'abcdefgh'
u%n = 0
u = h(u%s(1:5))
if (u%s /= 'abcde   ') error stop
if (u%n /= 5) error stop

! reached through a pointer, associated via another pointer that is then
! re-associated elsewhere: ptr1 still designates w, so w = f(ptr1%v) aliases
allocate(w%v(3))
w%v = [1.0, 2.0, 3.0]
allocate(z%v(2))
z%v = [9.0, 9.0]
ptr2 => w
ptr1 => ptr2
ptr2 => z
w = f(ptr1%v)
if (size(w%v) /= 3) error stop
if (any(w%v /= [1.0, 2.0, 3.0])) error stop
! z must not have been mistaken for the root of ptr1
if (size(z%v) /= 2) error stop
if (any(z%v /= [9.0, 9.0])) error stop

end program
