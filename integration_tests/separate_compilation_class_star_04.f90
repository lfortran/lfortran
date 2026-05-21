program separate_compilation_class_star_04
    use separate_compilation_class_star_04_user, only: work
    implicit none
    integer :: id1, id2
    call work(id1, id2)
    if (id1 /= 11) error stop
    if (id2 /= 22) error stop
end program separate_compilation_class_star_04
