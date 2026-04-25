module worker_mod
    use data_mod, only: global_count, global_values
    implicit none
contains

    subroutine set_count(n)
        integer, intent(out) :: n
        n = 5
    end subroutine set_count

    subroutine fill_values(arr, n)
        integer, intent(in) :: n
        real, intent(out) :: arr(n)
        integer :: i
        do i = 1, n
            arr(i) = real(i * 10)
        end do
    end subroutine fill_values

    subroutine do_work()
        call set_count(global_count)
        call fill_values(global_values, global_count)
    end subroutine do_work

end module worker_mod
