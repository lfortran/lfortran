program allocate_55
implicit none

type :: item_t
  class(*), allocatable :: value
end type

type(item_t), target :: original
type(item_t), pointer :: copy
integer :: val

allocate(original%value, source=42)

allocate(copy, source=original)

select type (v => copy%value)
type is (integer)
    val = v
end select
if (val /= 42) error stop

deallocate(copy)
print *, "ok"
end program
