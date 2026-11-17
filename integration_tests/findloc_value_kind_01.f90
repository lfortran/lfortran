program findloc_value_kind_01
! findloc with a VALUE whose type or kind differs from ARRAY's: complex
! and logical values of another kind, complex arrays with real or integer
! values and real or integer arrays with complex values, at run time and
! in PARAMETER initializers
implicit none
complex(4), parameter :: z4p(4) = [(1.0,0.0), (2.0,0.0), (3.0,0.0), (2.0,0.0)]
complex(8), parameter :: z8p(4) = [(1d0,0d0), (2d0,0d0), (3d0,0d0), (2d0,0d0)]
real(4), parameter :: x4p(4) = [1.0, 2.0, 3.0, 2.0]
real(8), parameter :: x8p(4) = [1d0, 2d0, 3d0, 2d0]
logical(4), parameter :: l4p(4) = [.false., .true., .false., .true.]
integer, parameter :: p1(1) = findloc(z4p, (2d0,0d0))
integer, parameter :: p2(1) = findloc(z8p, (2.0,0.0), back=.true.)
integer, parameter :: p3(1) = findloc(z4p, 2d0)
integer, parameter :: p4(1) = findloc(z8p, 2.0, mask=[.true.,.false.,.true.,.true.])
integer, parameter :: p5(1) = findloc(x8p, (2.0,0.0))
integer, parameter :: p6(1) = findloc(x4p, (2.0,0.0), back=.true.)
integer, parameter :: p10 = findloc(z8p, (3.0,0.0), dim=1)
complex(4) :: z4(4), zz(2,3)
complex(8) :: z8(4)
real(4) :: x4(4)
real(8) :: x8(4)
integer(4) :: i4(4)
integer(8) :: i8(4)
logical(4) :: l4(4)
logical(1) :: l1(4)
logical(8) :: l8(4)
complex(8) :: v8
real(8) :: r8
real(4), allocatable :: ra4
logical(1), allocatable :: la1
integer(4), pointer :: pi4
integer(4), target :: ti4
z4 = z4p; z8 = z8p; x4 = x4p; x8 = x8p
i4 = [1, 2, 3, 2]; i8 = i4
l4 = l4p; l1 = l4p; l8 = l4p
! complex value of another complex kind
if (any(findloc(z4, (2d0,0d0)) /= 2)) error stop 1
if (any(findloc(z8, (2.0,0.0), back=.true.) /= 4)) error stop 2
if (any(findloc(z4, (5d0,0d0)) /= 0)) error stop 3
v8 = (3d0,0d0)
if (findloc(z4, v8, dim=1) /= 3) error stop 4
! complex array, real or integer value
if (any(findloc(z4, 2d0) /= 2)) error stop 5
if (any(findloc(z8, 2.0, mask=[.true.,.false.,.true.,.true.]) /= 4)) error stop 6
if (findloc(z4, 2, dim=1) /= 2) error stop 7
if (any(findloc(z8, 2_8) /= 2)) error stop 8
r8 = 3d0
if (any(findloc(z4, r8) /= 3)) error stop 9
! real or integer array, complex value
if (any(findloc(x4, (2.0,0.0)) /= 2)) error stop 10
if (any(findloc(x8, (2.0,0.0), back=.true.) /= 4)) error stop 11
if (any(findloc(i4, (2.0,0.0)) /= 2)) error stop 12
if (any(findloc(i8, (3d0,0d0), mask=[.true.,.true.,.false.,.true.]) /= 0)) error stop 14
! logical value of another logical kind
if (any(findloc(l4, .true._1) /= 2)) error stop 15
if (any(findloc(l1, .true.) /= 2)) error stop 16
if (any(findloc(l4, .true._8, back=.true.) /= 4)) error stop 17
if (any(findloc(l8, .false._1) /= 1)) error stop 18
! rank 2 with dim
zz = reshape([(1.0,0.0), (2.0,0.0), (3.0,0.0), (2.0,0.0), (5.0,0.0), (2.0,0.0)], [2, 3])
if (any(findloc(zz, (2d0,0d0)) /= [2, 1])) error stop 19
if (any(findloc(zz, 2d0, dim=1) /= [2, 2, 2])) error stop 20
if (any(findloc(zz, 2_8, dim=2, back=.true.) /= [0, 3])) error stop 21
! allocatable and pointer scalar values of another type or kind
allocate(ra4, la1)
ra4 = 3.0; la1 = .true.; ti4 = 2
pi4 => ti4
if (any(findloc(x8, ra4) /= 3)) error stop 22
if (any(findloc(z8, ra4) /= 3)) error stop 23
if (any(findloc(l8, la1, back=.true.) /= 4)) error stop 24
if (any(findloc(x8, pi4) /= 2)) error stop 25
if (any(findloc(z4, pi4, back=.true.) /= 4)) error stop 26
! PARAMETER folding
if (p1(1) /= 2) error stop 31
if (p2(1) /= 4) error stop 32
if (p3(1) /= 2) error stop 33
if (p4(1) /= 4) error stop 34
if (p5(1) /= 2) error stop 35
if (p6(1) /= 4) error stop 36
if (p10 /= 3) error stop 40
print *, "ok"
end program
