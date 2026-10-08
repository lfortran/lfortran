program traits_procedure_value_02
    use traits_procedure_value_02_facade, only: mean => renamed
    implicit none
    abstract interface
        function average_real(x) result(r)
            real(8), intent(in) :: x(:)
            real(8) :: r
        end function
    end interface
    procedure(average_real), pointer :: average
    average => mean{real(8)}
    if (abs(average([1.d0,2.d0,3.d0,4.d0,5.d0]) - 3.d0) > 1.d-12) error stop 1
    associate(integer_average => mean{integer})
        if (integer_average([1,2,3,4,5]) /= 3) error stop 2
    end associate
end program
