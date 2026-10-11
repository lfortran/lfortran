program class_nominal_separate_01
    use class_nominal_separate_01_a_m, only: A => Payload, accept_a
    use class_nominal_separate_01_b_m, only: exercise_b
    implicit none
    type(A) :: first
    first%value = 11
    call accept_a(first)
    call exercise_b()
end program class_nominal_separate_01
