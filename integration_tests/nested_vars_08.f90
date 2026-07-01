module nested_vars_08_mod
    implicit none
contains
    logical function run_case() result(success)
        character(*), parameter :: prefix = "[roundtrip] "

        success = .true.

        first_block: block
            integer :: left, right

            left = 1
            right = 1
            if (.not. same_value(left, right, "first")) then
                success = .false.
                return
            end if
        end block first_block

        second_block: block
            integer :: left, right

            left = 2
            right = 2
            if (.not. same_value(left, right, "second")) then
                success = .false.
                return
            end if
        end block second_block

    contains
        logical function same_value(left, right, name) result(ok)
            integer, intent(in) :: left
            integer, intent(in) :: right
            character(*), intent(in) :: name

            ok = left == right
            if (.not. ok) print *, prefix // name
        end function
    end function
end module

program nested_vars_08
    use nested_vars_08_mod, only: run_case
    implicit none

    if (.not. run_case()) error stop
end program
