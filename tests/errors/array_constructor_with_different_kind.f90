program array_constructor_with_different_kind
    real(8), allocatable :: x(:)
    allocate(x(4))
    print *, [x, [1., 2.]]
end program
