module scalar_reduction_actual
    implicit none
contains
    pure elemental real function rounded(value) result(r)
        real, intent(in) :: value
        r = real(int(value))
    end function

    pure subroutine partitioned(x, input, y, vector, parts)
        real, contiguous, intent(in) :: x(:,:), input(:,:)
        real, contiguous, intent(out) :: y(:,:)
        logical, intent(in) :: vector
        integer, intent(in) :: parts
        integer :: width, partition, groups, n
        width = size(x,1)
        partition = width / parts
        groups = width / 64
        do concurrent (n = 1:size(y,1))
            block
                real :: codes(width), weights(width)
                integer :: d, m
                do concurrent (d = 1:width)
                    codes(d) = input(d,n)
                    if (.not. vector) weights(d) = rounded(codes(d))
                end do
                do concurrent (m = 1:size(x,2))
                    block
                        real :: partial_values(parts), terms(groups), total
                        integer :: g, p
                        if (vector) then
                            do concurrent (g = 1:groups)
                                terms(g) = sum(codes((g-1)*64+1:g*64)*x((g-1)*64+1:g*64,m))
                            end do
                            total = sum(terms)
                        else if (parts == 1) then
                            total = sum(weights*x(:,m))
                        else
                            do concurrent (p = 1:parts)
                                partial_values(p) = rounded(sum( &
                                    weights((p-1)*partition+1:p*partition)*x((p-1)*partition+1:p*partition,m)))
                            end do
                            total = sum(partial_values)
                        end if
                        y(n,m) = rounded(total)
                    end block
                end do
            end block
        end do
    end subroutine
end module

program gpu_scalar_reduction_actual_01
    use scalar_reduction_actual
    implicit none
    real :: x(128,17), input(128,3), y(3,17), expected
    integer :: i, j, rows, parts, p
    do j = 1,17
        do i = 1,128
            x(i,j) = real(mod(i+j,5)-2)
        end do
    end do
    do j = 1,3
        do i = 1,128
            input(i,j) = real(mod(i+2*j,7)-3)
        end do
    end do
    do rows = 1,17,16
        do parts = 1,4
            if (mod(128,parts) /= 0) cycle
            call partitioned(x(:,:rows),input,y(:,:rows),rows==1,parts)
            do j = 1,rows
                do i = 1,3
                    expected = 0
                    do p = 1,128
                        expected = expected + input(p,i)*x(p,j)
                    end do
                    if (y(i,j) /= expected) error stop 1
                end do
            end do
        end do
    end do
    call partitioned(x,input,y(:0,:),.false.,2)
    print *, "ok"
end program
