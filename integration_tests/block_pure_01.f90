module block_pure_01_m
    implicit none
contains
    pure function nested_sum(values) result(total)
        integer, intent(in) :: values(:)
        integer :: total
        total = 0
        block
            integer :: i
            do i = 1, size(values)
                associate(value => values(i))
                    block
                        total = total + twice(value)
                    end block
                end associate
            end do
        end block
    end function
    pure integer function twice(value)
        integer, intent(in) :: value
        twice = 2 * value
    end function
end module

program block_pure_01
    use block_pure_01_m
    implicit none
    if (nested_sum([3, 5, 7]) /= 30) error stop 1
end program
