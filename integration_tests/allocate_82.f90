program allocate_82
! A failed ALLOCATE with STAT= assigns a positive value to the stat
! variable and leaves the object unallocated (#17300). Each request is
! so large that malloc fails at once, without committing any memory.
implicit none
type :: t
    real(8) :: x(4)
end type
type :: holder
    real(8), allocatable :: x(:)
end type
real(8), allocatable :: a(:), b(:,:), small(:)
real(8), pointer :: p(:) => null()
real(8), target :: tgt(5)
character(len=8), pointer :: cp(:) => null()
character(len=8), target :: ctgt(3)
character(:), allocatable :: s
character(len=8), allocatable :: sa(:)
type(t), allocatable :: ta(:)
class(t), allocatable :: ca(:)
class(*), allocatable :: ua(:)
character(:), allocatable :: da(:)
type(holder) :: h
integer(8) :: n
integer :: ierr

n = huge(n) / 16

ierr = 0
allocate(a(n), stat=ierr)
if (ierr <= 0) error stop "1D: stat not set"
if (allocated(a)) error stop "1D: allocated"

ierr = 0
allocate(b(2_8**29, 2_8**29), stat=ierr)
if (ierr <= 0) error stop "2D: stat not set"
if (allocated(b)) error stop "2D: allocated"

ierr = 0
allocate(character(len=huge(n)/2) :: s, stat=ierr)
if (ierr <= 0) error stop "character(:): stat not set"
if (allocated(s)) error stop "character(:): allocated"

ierr = 0
allocate(sa(n), stat=ierr)
if (ierr <= 0) error stop "character array: stat not set"
if (allocated(sa)) error stop "character array: allocated"

ierr = 0
allocate(ta(n / 4), stat=ierr)
if (ierr <= 0) error stop "derived-type array: stat not set"
if (allocated(ta)) error stop "derived-type array: allocated"

ierr = 0
allocate(t :: ca(n / 4), stat=ierr)
if (ierr <= 0) error stop "class array: stat not set"
if (allocated(ca)) error stop "class array: allocated"

ierr = 0
allocate(p(n), stat=ierr)
if (ierr <= 0) error stop "pointer array: stat not set"
if (associated(p)) error stop "pointer array: associated"

! The standard requires a pointer that is not successfully allocated to
! retain its previous association, but some processors (gfortran)
! disassociate it. Accept either, but not a half-updated pointer.
p => tgt
ierr = 0
allocate(p(n), stat=ierr)
if (ierr <= 0) error stop "associated pointer array: stat not set"
if (associated(p)) then
    if (.not. associated(p, tgt)) error stop "associated pointer array: new target"
    if (size(p) /= 5) error stop "associated pointer array: wrong size"
end if

ierr = 0
allocate(cp(n), stat=ierr)
if (ierr <= 0) error stop "character pointer array: stat not set"
if (associated(cp)) error stop "character pointer array: associated"

ctgt = "abc"
cp => ctgt
ierr = 0
allocate(cp(n), stat=ierr)
if (ierr <= 0) error stop "associated character pointer array: stat not set"
if (associated(cp)) then
    if (.not. associated(cp, ctgt)) error stop "associated character pointer array: new target"
    if (size(cp) /= 3) error stop "associated character pointer array: wrong size"
    if (cp(3) /= "abc") error stop "associated character pointer array: wrong value"
end if

ierr = 0
allocate(h%x(n), stat=ierr)
if (ierr <= 0) error stop "component: stat not set"
if (allocated(h%x)) error stop "component: allocated"

ierr = 0
allocate(real(8) :: ua(n), stat=ierr)
if (ierr <= 0) error stop "class(*) array: stat not set"
if (allocated(ua)) error stop "class(*) array: allocated"

ierr = 0
allocate(character(len=8) :: da(n), stat=ierr)
if (ierr <= 0) error stop "deferred-length character array: stat not set"
if (allocated(da)) error stop "deferred-length character array: allocated"

ierr = 0
allocate(a(n), source=1.0d0, stat=ierr)
if (ierr <= 0) error stop "source=: stat not set"
if (allocated(a)) error stop "source=: allocated"

! The objects allocated before the failing one stay allocated.
ierr = 0
allocate(small(10), a(n), stat=ierr)
if (ierr <= 0) error stop "list: stat not set"
if (.not. allocated(small)) error stop "list: first object not allocated"
if (allocated(a)) error stop "list: failing object allocated"

! A later ALLOCATE that succeeds assigns zero.
ierr = -1
allocate(a(10), stat=ierr)
if (ierr /= 0) error stop "stat not zero after a successful allocate"
if (.not. allocated(a)) error stop "not allocated after a successful allocate"
a = 1
if (sum(a) /= 10) error stop "wrong values"
print *, "ok"
end program
