! Test: allocate(child_t :: s) must preserve dynamic type when subroutine
! has an assumed-shape array parameter alongside a polymorphic allocatable.
type :: base_t
end type
type, extends(base_t) :: child_t
end type

class(base_t), allocatable :: s
call make(s, [1])

select type(s)
type is (child_t)
    print *, "PASSED"
type is (base_t)
    error stop "FAIL: select type matched base_t instead of child_t"
end select

contains
subroutine make(s, arr)
    class(base_t), allocatable, intent(out) :: s
    integer, intent(in) :: arr(:)
    allocate(child_t :: s)
end subroutine
end
