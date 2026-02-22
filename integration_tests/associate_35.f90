program associate_35
    implicit none
    real :: x(3)
    x = [1.0, 2.0, 3.0]
    call s(x)
contains
    subroutine s(input_data)
        real, intent(in) :: input_data(*)
        ! TODO(#10218): Use `input_data` directly once codegen
        ! supports assumed-size ASSOCIATE targets without ICE.
        associate(a => input_data(:3))
            print *, a(1)
        end associate
    end subroutine s
end program associate_35
