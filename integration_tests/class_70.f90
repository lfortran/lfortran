program class_70
    type :: temp_type
        integer :: x
    end type
    class(*), allocatable :: a, b, c
    allocate(a, source=temp_type(3))
    allocate(b, source=temp_type(5))
    ! c = b     !! Handle allocation in `check_and_allocate_scalar` in llvm
    ! select type (c)
    ! type is (temp_type)
    !     if (c%x /= 5) error stop
    ! end select
    b = a
    select type (b)
    type is (temp_type)
        if (b%x /= 3) error stop
    end select
end program