program atomic
    use iso_fortran_env
    integer :: atom[*]
    call atomic_add (atom[1], this_image())
end program atomic