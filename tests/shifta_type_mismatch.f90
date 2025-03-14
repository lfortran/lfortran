program shifta_type_mismatch
    integer(8) :: x = 32
    integer :: y = 2
    x = shifta(x, y)
    print *, x
end program shifta_type_mismatch
