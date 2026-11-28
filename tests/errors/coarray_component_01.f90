program coarray_component_01
    implicit none
    type t
        integer, allocatable :: caf[:]
    end type
    type(t) :: a
    allocate(a%caf[3:*])
    if (any(lcobound(a%caf) /= [3])) error stop 1
end program coarray_component_01
