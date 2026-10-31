program intrinsic_pack_allocatable_02
implicit none
integer :: source_calls = 0, mask_calls = 0, vector_calls = 0
integer, allocatable :: result(:)
result = pack(source(source_calls), mask(mask_calls))
if (source_calls /= 1 .or. mask_calls /= 1) error stop 1
if (size(result) /= 2) error stop 2
if (result(1) /= 10 .or. result(2) /= 30) error stop 3
result = pack(result, result > 15)
if (size(result) /= 1) error stop 4
if (result(1) /= 30) error stop 5
result = pack(result, result < 0)
if (.not. allocated(result)) error stop 6
if (size(result) /= 0) error stop 7
if (source_calls /= 1 .or. mask_calls /= 1) error stop 8
result = pack(source(source_calls), mask(mask_calls), vector(vector_calls))
if (source_calls /= 2 .or. mask_calls /= 2 .or. vector_calls /= 1) error stop 9
if (size(result) /= 4) error stop 10
if (result(1) /= 10 .or. result(2) /= 30) error stop 11
if (result(3) /= -3 .or. result(4) /= -4) error stop 12
deallocate(result)
allocate(result(-1:2))
result = pack(source(source_calls), .true., vector(vector_calls))
if (source_calls /= 3 .or. vector_calls /= 2) error stop 13
if (lbound(result, 1) /= -1 .or. ubound(result, 1) /= 2) error stop 14
if (result(-1) /= 10 .or. result(0) /= 20 .or. result(1) /= 30) error stop 15
if (result(2) /= -4) error stop 16
result = pack(result, result > 15)
if (lbound(result, 1) /= 1 .or. size(result) /= 2) error stop 17
if (result(1) /= 20 .or. result(2) /= 30) error stop 18
contains
function source(calls) result(a)
integer, intent(inout) :: calls
integer :: a(3)
calls = calls + 1
a(1) = 10
a(2) = 20
a(3) = 30
end function
function mask(calls) result(m)
integer, intent(inout) :: calls
logical :: m(3)
calls = calls + 1
m(1) = .true.
m(2) = .false.
m(3) = .true.
end function
function vector(calls) result(a)
integer, intent(inout) :: calls
integer :: a(4)
calls = calls + 1
a(1) = -1
a(2) = -2
a(3) = -3
a(4) = -4
end function
end program
