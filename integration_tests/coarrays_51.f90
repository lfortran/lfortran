program coarrays_51
    use coarrays_51_a
    use coarrays_51_b
    implicit none
    integer :: me

    me = this_image()

    ! Allocated and holding their initial values before the first statement,
    ! by the initializer the modules' own object file defines.
    if (ca /= 11) error stop 1
    if (cb /= 22) error stop 2
    if (pts(1)%x /= 5) error stop 3
    if (pts(2)%x /= 5) error stop 4
    if (associated(p)) error stop 5

    ca = 11 + me
    cb = 22 + me
    sync all

    ! Read through the companions the defining object file allocated: a
    ! second binding in this unit would point these at storage no other
    ! image ever wrote.
    if (ca[1] /= 12) error stop 6
    if (cb[1] /= 23) error stop 7
end program
