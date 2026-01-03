program intrinsics_302
    implicit none
    integer :: x(4) = [1,2,3,4]

    print *, is_contiguous([1,2,3,4])
    if (.not. is_contiguous([1,2,3,4])) error stop
    
    print *, is_contiguous(x)
    if (.not. is_contiguous(x)) error stop
    
    print *, is_contiguous(x(::2))
    if (is_contiguous(x(::2))) error stop
end program
  