program function_31
    integer :: x(2,3) = reshape([1, 2, 3, 4, 5, 6], [2,3])
    call func(x)
  contains 
    subroutine func(x)
        integer, intent(in) :: x(:, :)
        integer :: y2(size(x, 2), size(x, 1) + 1)
        y2 = reshape([allocate_return_type_func(x), x(1,:)], shape(y2))
        print *, y2
        if(any(y2(:, 1) /= [1,2,3]) .or. any(y2(:, 2) /= [4,5,6]) .or. any(y2(:, 3) /= [1,3,5])) error stop
    end subroutine
    function allocate_return_type_func(x) result(z)
        integer, intent(in) :: x(:, :)
        integer, allocatable :: z(:, :) 
        allocate(z(size(x,1), size(x,2)))
        z = x
    end function
end program 