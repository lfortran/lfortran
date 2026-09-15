! Separate compilation of initializers from a use-imported derived-type
! parameter whose type has a component of a derived type that
! derived_types_166_c never imports; the .mod file of derived_types_166_c
! carries the import of that nested type.
program derived_types_166
    use derived_types_166_c
    implicit none
    type(h) :: x
    type(h2) :: y
    if (x%c%i /= 7 .or. x%c%n%k /= 3) error stop 1
    if (y%d%i /= 7 .or. y%d%n%k /= 3) error stop 2
    if (mv%i /= 7 .or. mv%n%k /= 3) error stop 3
    if (g() /= 7) error stop 4
    print *, "ok"
end program
