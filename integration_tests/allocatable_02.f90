! Renamed use-associated scalar allocatables: allocation references
! resolve to the ultimate variable, matching its declaration and flag.
module alloc_mod_02
implicit none
character(len=:), allocatable :: gs
integer, allocatable :: gn
end module alloc_mod_02

program allocatable_02
use alloc_mod_02, only: s => gs, n => gn
implicit none
if (allocated(s)) error stop 1
if (allocated(n)) error stop 2
allocate(character(len=2) :: s)
s = "hi"
if (.not. allocated(s)) error stop 3
if (s /= "hi") error stop 4
allocate(n)
n = 42
if (.not. allocated(n)) error stop 5
if (n /= 42) error stop 6
print *, s, n
deallocate(s, n)
if (allocated(s)) error stop 7
if (allocated(n)) error stop 8
end program allocatable_02
