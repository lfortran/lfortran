program intrinsics_303
    integer :: a
    call random_seed(a)
    print *, a
    if (a /= 8) error stop
end program