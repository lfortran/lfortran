! Allocation status is independent of the stored scalar or array values.
program allocatable_01
implicit none
integer, allocatable :: a(:)
integer, allocatable :: n
character(len=:), allocatable :: s
character(len=:), allocatable :: u
print *, allocated(a)
if (allocated(a)) error stop 1
allocate(a(3))
a(1) = 1
a(2) = 2
a(3) = 3
if (.not. allocated(a)) error stop 2
if (a(2) /= 2) error stop 3
call fill(s)
print *, s
if (s /= "hi") error stop 4
if (allocated(n)) error stop 10
allocate(n)
if (.not. allocated(n)) error stop 11
n = 42
if (.not. allocated(n)) error stop 12
if (n /= 42) error stop 13
deallocate(n)
if (allocated(n)) error stop 14
n = 7
if (.not. allocated(n)) error stop 15
if (n /= 7) error stop 16
if (allocated(u)) error stop 20
allocate(character(len=5) :: u)
if (.not. allocated(u)) error stop 21
u = "hello"
if (.not. allocated(u)) error stop 22
if (u /= "hello") error stop 23
deallocate(u)
if (allocated(u)) error stop 24
u = "auto"
if (.not. allocated(u)) error stop 25
if (u /= "auto") error stop 26
contains
subroutine fill(t)
character(len=:), allocatable, intent(out) :: t
t = "hi"
end subroutine fill
end program allocatable_01
