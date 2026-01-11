program unlimited_poly_test
    implicit none
    class(*), allocatable :: u

    allocate(integer :: u)
    select type(u)
        type is (integer)
            u = 42
            print *, "It is an integer!"
            print *, "Value is:", u
            if (u /= 42) error stop
        class default
            print *, "Unknown type"
            error stop
    end select
end program unlimited_poly_test