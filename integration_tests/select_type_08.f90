program select_type_08
    implicit none

    type base
        integer :: x
    end type

    integer :: case_int

    class(*), allocatable :: x

    allocate(x, source = 10)
    call f(x, case_int)
    if (case_int /= 0) error stop

    deallocate(x)

    allocate(x, source = base(10))
    call f(x, case_int)
    if (case_int /= 1) error stop

contains

    subroutine f(generic, selected_case)
        class(*) :: generic
        integer, intent(out) :: selected_case

        select type(generic)
            type is (integer)
                print *, generic
                if (generic /= 10) error stop
                selected_case = 0
            type is (base)
                print *, generic%x
                if (generic%x /= 10) error stop
                selected_case = 1
            class default
                error stop
        end select
    end subroutine f
end program select_type_08
