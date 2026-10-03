module fortran_03_mod
    implicit none
    integer :: total = 0
contains
    ! The declarations before the interface block are longer than a line
    ! together, but each fits on one.
    subroutine apply_twice(f)
        interface
            subroutine f()
            end subroutine f
        end interface
        integer :: first_local_counter_variable
        integer :: second_local_counter_variable
        integer :: third_local_counter_variable
        first_local_counter_variable = 1
        second_local_counter_variable = 2
        third_local_counter_variable = 3
        call f()
        call f()
        total = total + first_local_counter_variable + second_local_counter_variable &
            + third_local_counter_variable
    end subroutine apply_twice

    subroutine incr()
        total = total + 1
    end subroutine incr
end module fortran_03_mod

program fortran_03
    use fortran_03_mod, only: apply_twice, incr, total
    implicit none
    call apply_twice(incr)
    if (total /= 8) error stop
    print *, total
end program fortran_03
