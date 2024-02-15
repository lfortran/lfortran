program types_real_array_to_complex_array_cast
    complex(8) :: D(3) = [1.0, 2.0, 3.0]
    if (any(D /= [(1.0, 0.0), (2.0, 0.0), (3.0, 0.0)])) error stop
end program
