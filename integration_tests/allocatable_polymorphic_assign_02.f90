program allocatable_polymorphic_assign_02
    ! F2003 allocate-on-assignment: an unallocated class(*), allocatable
    ! array should be allocated automatically with the dynamic type and
    ! shape of the RHS.
    implicit none
    class(*), allocatable :: x(:)
    x = [1, 2, 3, 4, 5]
    select type (x)
    type is (integer)
        if (size(x) /= 5) error stop "size"
        if (x(1) /= 1) error stop "v1"
        if (x(5) /= 5) error stop "v5"
    class default
        error stop "type"
    end select
    print *, "PASS"
end program allocatable_polymorphic_assign_02
