! This test ensures that reshape's return gets
! assigned correctly to the LHS if it's `PointerToData`-physicalType array.
program arrays_78
    integer :: x(2,3) = reshape([1, 2, 3, 4, 5, 6], [2,3])
    call func(x)
  contains 
    subroutine func(x)
       integer, intent(in) :: x(:, :)
       integer :: y(size(x, 2), size(x, 1))
         print *, reshape(x, [3,2])
         y = reshape(x, shape(y))
         print *, y
         if(any(y /= reshape([1, 2, 3, 4, 5, 6], [3,2]))) error stop
    end subroutine
  end program arrays_78
  