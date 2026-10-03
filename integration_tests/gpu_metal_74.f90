program gpu_metal_74
! Test: do concurrent inside associate block where associate name is bound
! to an allocatable derived-type member. Previously caused ASR verify failure
! in the gpu_offload pass because ExternalSymbol entries for struct members
! were not found when the do concurrent was inside an AssociateBlock scope.
implicit none
type :: t
  integer, allocatable :: nodes(:)
end type
type(t) :: x
integer, allocatable :: vals(:)
integer :: i, n

allocate(x%nodes(3), vals(3))
x%nodes = [10, 20, 30]
vals = 0
n = 3

call check(x, vals, n)

if (vals(1) /= 10) error stop
if (vals(2) /= 20) error stop
if (vals(3) /= 30) error stop
print *, vals(1), vals(2), vals(3)

contains
  subroutine check(arg, out, sz)
    type(t), intent(in) :: arg
    integer, intent(inout) :: out(:)
    integer, intent(in) :: sz
    integer :: l
    associate(nd => arg%nodes)
      do concurrent(l = 1:sz)
        out(l) = nd(l)
      end do
    end associate
  end subroutine
end program
