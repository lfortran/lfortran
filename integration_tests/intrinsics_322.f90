program intrinsics_322
    character(3) :: cx(2)
    cx(1) = "woo"
    cx(2) = "oba"
    print *, index(cx, "a")
    if (any(index(cx, "a") /= [0, 3])) error stop
end program