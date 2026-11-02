module pure_side_effects_05_mod
    implicit none
    integer :: input = 7
contains
    pure integer function compute() result(res)
        integer :: value
        res = input
        value = 2
        block
            integer :: input
            input = 3
            value = value + input
        end block
        call set_result(res)
        res = res + value + read_host() + input
    contains
        pure integer function read_host() result(answer)
            answer = value
            block
                integer :: res
                res = 1
                answer = answer + res
            end block
        end function

        pure subroutine set_result(output)
            integer, intent(out) :: output
            output = 20
        end subroutine
    end function

    integer function impure_host() result(res)
        res = 0
        call set_host()
    contains
        subroutine set_host()
            res = 9
        end subroutine
    end function
end module

program pure_side_effects_05
    use pure_side_effects_05_mod
    implicit none
    if (compute() /= 38) error stop
    if (impure_host() /= 9) error stop
end program
