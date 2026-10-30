program intrinsic_pack_allocatable_03
implicit none
integer :: source(1)
integer, allocatable :: dest(:)
source = 7
allocate(dest(-2:-2))
dest = pack(source, .true.)
if (lbound(dest, 1) /= -2 .or. ubound(dest, 1) /= -2) error stop 1
if (dest(-2) /= 7) error stop 2
end program
