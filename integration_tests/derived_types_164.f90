! Separate compilation of entities initialized from a use-imported
! derived-type parameter. derived_types_164_c imports `t` as `u` and, in a
! procedure, as `t`; neither makes it export `t`, so the `t` of
! derived_types_164_b is the only one visible here.
program derived_types_164
    use derived_types_164_c
    use derived_types_164_b
    implicit none
    type(t) :: y
    type(holder) :: h
    if (mv%i /= 7 .or. mv%j /= 8) error stop 1
    if (mvk%i /= 4 .or. mvk%j /= 1) error stop 2
    if (mp%i /= 7 .or. mp%j /= 8) error stop 3
    if (.not. associated(ptr)) error stop 4
    if (ptr%i /= 5 .or. ptr%j /= 6) error stop 5
    if (h%c%i /= 7 .or. h%c%j /= 8) error stop 6
    if (local_value() /= 15) error stop 7
    if (abs(y%r - 2.5) > 1e-6) error stop 8
    print *, "ok"
end program
