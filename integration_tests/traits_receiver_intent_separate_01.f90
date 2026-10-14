! The providers define same-named, unrelated Counter types; the consumer,
! compiled without them, reads, mutates and resets each object exactly once
! per run in either selection order, then mutates both through a pointer.
program traits_receiver_intent_separate_01
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use traits_receiver_intent_separate_01_contracts_m, only: ICounter
    use traits_receiver_intent_separate_01_consumer_m, only: choose, nudge
    use traits_receiver_intent_separate_01_a_m, A => Counter
    use traits_receiver_intent_separate_01_b_m, B => Counter
    implicit none
    type(A), target :: first
    type(B), target :: second
    class(ICounter), pointer :: p
    type(c_ptr) :: first_address, second_address
    integer :: i
    first%n = 11
    second%n = 10
    second%scale = 3
    first_address = c_loc(first%n)
    second_address = c_loc(second%n)
    do i = 0, 1
        call choose(mod(command_argument_count() + i, 2), first, second)
    end do
    if (a_finals /= 1 .or. a_finals_with_log /= 1) error stop 801
    if (first%n /= 4 .or. .not. allocated(first%log)) error stop 802
    if (size(first%log) /= 0) error stop 803
    if (second%n /= 4 .or. second%scale /= 2) error stop 804
    if (.not. c_associated(a_reset_seen, first_address)) error stop 805
    if (.not. c_associated(b_reset_seen, second_address)) error stop 806
    p => first
    call nudge(p, 2)
    p => second
    call nudge(p, 3)
    if (first%n /= 6 .or. size(first%log) /= 1 .or. second%n /= 7) error stop 807
    if (.not. c_associated(a_bump_seen, first_address)) error stop 808
    if (.not. c_associated(b_bump_seen, second_address)) error stop 809
    if (a_finals /= 1) error stop 810
    print '(a)', 'traits_receiver_intent_separate_01 ok'
end program traits_receiver_intent_separate_01
