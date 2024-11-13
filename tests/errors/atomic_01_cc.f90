program atomic_01_cc
    use iso_fortran_env
    integer :: atom[*]
    call atomic_add (atom[1], this_image())
    call atomic_add (atom[2], this_image())
    print *, "compilation continued despite errors"
end program