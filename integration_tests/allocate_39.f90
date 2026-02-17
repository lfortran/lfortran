program allocate_39
    implicit none
    class(*), allocatable :: obj
    integer :: mold_var

    mold_var = 0
    allocate(obj, mold=mold_var)
    select type(obj)
        type is (integer)
            obj = 55
            print *, obj
            if (obj /= 55) error stop
        class default
            error stop
    end select

    print *, "PASS"
end program allocate_39
