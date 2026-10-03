program gpu_metal_75
! Test: VLA dimension referencing a decomposed struct member.
! When --gpu=metal decomposes a struct into flat-array kernel params,
! VLA dimensions like size(x%nodes) must be rewritten to reference
! the decomposed parameter.
implicit none
type :: t
    integer, allocatable :: nodes(:)
end type
type(t) :: a
allocate(a%nodes(3))
a%nodes = [1, 2, 3]
if (f(a)) then
    print *, "OK"
else
    error stop
end if
contains
function f(x) result(res)
    type(t), intent(in) :: x
    logical :: res
    integer :: i
    logical :: eq(size(x%nodes))
    do concurrent(i = 1:size(x%nodes))
        eq(i) = (x%nodes(i) > 0)
    end do
    res = all(eq)
end function
end program
