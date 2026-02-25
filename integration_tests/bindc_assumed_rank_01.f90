module bindc_assumed_rank_01_mod
    use iso_c_binding, only: c_int, c_ptr, c_null_ptr, c_loc
    implicit none

contains
    subroutine c_process(a, n) bind(C, name="c_process_impl")
        type(*) :: a(..)
        integer(c_int), value :: n
    end subroutine

    subroutine do_process(a)
        type(*), intent(inout), target :: a(..)
        integer(c_int) :: n
        n = 1
        call c_process(a, n)
    end subroutine
end module

program bindc_assumed_rank_01
    use bindc_assumed_rank_01_mod
    implicit none
    print *, "ok"
end program
