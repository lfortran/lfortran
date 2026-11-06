module gpu_nested_block_helpers
implicit none
contains
    pure real function offset(x)
        real, intent(in) :: x
        offset = x + 1.0
    end function

    pure real function evaluate(x)
        real, intent(in) :: x
        block
            real :: local
            local = offset(x)
            evaluate = 2.0 * local
        end block
    end function
end module

module gpu_nested_block_kernel
use gpu_nested_block_helpers, only: evaluate
implicit none
contains
    subroutine apply(x, y)
        real, intent(in) :: x(3, 2)
        real, intent(out) :: y(3, 2)
        integer :: i, j
        do concurrent (i=1:2)
            block
                real :: bias
                bias = real(i)
                do concurrent (j=1:3)
                    block
                        real :: value
                        value = evaluate(x(j, i))
                        y(j, i) = value + bias
                    end block
                end do
            end block
        end do
    end subroutine
end module

program gpu_nested_block_calls_01
use gpu_nested_block_kernel, only: apply
implicit none
real :: x(3, 2), y(3, 2)
integer :: i, j
do i = 1, 2
    do j = 1, 3
        x(j, i) = real(3*i + j)
    end do
end do
call apply(x, y)
do i = 1, 2
    do j = 1, 3
        if (y(j, i) /= 2.0*(x(j, i) + 1.0) + real(i)) error stop
    end do
end do
print *, "nested block helper calls passed"
end program
