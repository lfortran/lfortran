program class_allocate_02
    implicit none

    type dummy_type
        integer(1), allocatable :: value(:)
    end type dummy_type

    class(*), allocatable :: dummy
    type(dummy_type) :: dummy_val

    allocate(dummy_val % value(1))
    allocate(dummy, source=dummy_val)

    print * , allocated(dummy)
    if (allocated(dummy) .neqv. .true.) error stop 
    
    !select type(d => dummy)
    !type is (dummy_type)
    !    print *, size(d % value)
    !    if (size(d % value) /= 1) error stop
    !end select
end program
