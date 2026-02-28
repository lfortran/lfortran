program verify_intent_external_01
    use data_mod, only: global_count, global_values
    use worker_mod, only: do_work
    implicit none

    call do_work()

    if (global_count /= 5) error stop
    if (abs(global_values(1) - 10.0) > 1.0e-6) error stop
    if (abs(global_values(5) - 50.0) > 1.0e-6) error stop

    print *, "ok"
end program verify_intent_external_01
