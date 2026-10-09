! The providers deliberately define same-named, same-layout unrelated types.
program traits_runtime_separate_01
    use traits_runtime_separate_01_consumer_m, only: choose
    use traits_runtime_separate_01_a_m, A => Payload
    use traits_runtime_separate_01_b_m, B => Payload
    implicit none
    type(A) :: first
    type(B) :: second
    integer :: i
    first%value = 11
    second%value = 11
    do i = 0, 1
        call choose(mod(command_argument_count() + i, 2), first, second)
    end do
end program traits_runtime_separate_01
