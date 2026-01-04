program class_90
    implicit none
    integer :: x(3)
    x = [1, 2, 3]
    call print_any(x)

contains

    subroutine print_any(val)
        class(*) :: val(:)
        select type(val)
        type is (integer)
            call print_any2(val)
            print *, "After modification in print_any2:", val(1), val(2), val(3)
            if (val(1) /= 10 .or. val(2) /= 20 .or. val(3) /= 30) error stop "Modification not reflected"
        end select
    end subroutine print_any

    subroutine print_any2(val)
        integer :: val(:)
        print *, "Integer array:", val
        if (size(val) /= 3) error stop "Size mismatch"
        if (val(1) /= 1 .or. val(2) /= 2 .or. val(3) /= 3) error stop "Value mismatch"
        val(1) = 10
        val(2) = 20
        val(3) = 30
    end subroutine print_any2

end program class_90