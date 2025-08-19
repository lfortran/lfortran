
module container_mod
    type, abstract :: base
    end type base
    type, extends(base) :: child
        integer :: x = 42
    end type child
    type :: container
        class(base), allocatable :: val
    end type container
end module container_mod

program select_type_12
    use container_mod
    type(container) :: c
    type(child) :: ch
    integer :: t
    class(container), allocatable :: self
    allocate(self)
    allocate(child::self%val)
    select type(val => self%val)
    type is(child)
        print *, "child%x =", val%x
        t=val%x
    end select
    if(t /= 42) then
        print *, "Error: t should be 42, but is", t
    else
        print *, "Success: t is", t
    end if
end program select_type_12