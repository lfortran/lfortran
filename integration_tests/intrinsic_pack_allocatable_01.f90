program intrinsic_pack_allocatable_01
implicit none
integer :: a(3)
logical :: mask(3)
logical :: select_all
integer, allocatable :: result(:)
a(1) = 10
a(2) = 20
a(3) = 30
mask(1) = .true.
mask(2) = .false.
mask(3) = .true.
result = pack(a, mask)
if (.not. allocated(result)) error stop 1
if (size(result) /= 2) error stop 2
if (result(1) /= 10 .or. result(2) /= 30) error stop 3
select_all = .true.
result = pack(a, select_all)
if (size(result) /= 3) error stop 4
if (result(1) /= 10 .or. result(2) /= 20 .or. result(3) /= 30) error stop 5
select_all = .false.
result = pack(a, select_all)
if (.not. allocated(result)) error stop 6
if (size(result) /= 0) error stop 7
end program
