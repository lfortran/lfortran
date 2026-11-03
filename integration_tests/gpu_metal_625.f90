! A strided section passed to a procedure in a `do concurrent` nested in a
! BLOCK inside another `do concurrent`.
program gpu_metal_625
    implicit none
    integer, parameter :: nh = 5, nd = 3, nk = 4
    integer :: p(nd,nk), h, d, k
    real :: out(nd,nh), sums(nd,nh), cols(nd,nh), assoc(nd,nh), expected
    do k = 1, nk
        do d = 1, nd
            p(d,k) = 10*d + k
        end do
    end do

    do concurrent (h = 1:nh)
        block
            integer :: partials(nd,nk)
            real :: factors(nk)
            integer :: i, j
            do i = 1, nk
                do j = 1, nd
                    partials(j,i) = h*i + j
                end do
                factors(i) = real(i)
            end do
            do concurrent (d = 1:nd)
                out(d,h) = sum(convert(partials(d,:))*factors)
            end do
        end block
    end do

    do concurrent (h = 1:nh)
        block
            do concurrent (d = 1:nd)
                sums(d,h) = total(p(d,:)) + real(h)
            end do
        end block
    end do

    ! The same nest inside an enclosing BLOCK.
    block
        integer :: shift
        shift = 7
        do concurrent (h = 1:nh)
            block
                do concurrent (d = 1:nd)
                    cols(d,h) = total(p(d,:)) + real(shift*h)
                end do
            end block
        end do
    end block

    ! And with an ASSOCIATE instead of the inner BLOCK.
    do concurrent (h = 1:nh)
        associate (offset => 2*h)
            do concurrent (d = 1:nd)
                assoc(d,h) = total(p(d,:)) + real(offset)
            end do
        end associate
    end do

    do h = 1, nh
        do d = 1, nd
            expected = 0
            do k = 1, nk
                expected = expected + real((h*k + d)*k)
            end do
            if (out(d,h) /= expected) error stop 1
            expected = real(h)
            do k = 1, nk
                expected = expected + real(p(d,k))
            end do
            if (sums(d,h) /= expected) error stop 2
            if (cols(d,h) /= expected + real(6*h)) error stop 3
            if (assoc(d,h) /= expected + real(h)) error stop 4
        end do
    end do
    print *, "ok"
contains
    pure elemental real function convert(bits)
        integer, intent(in) :: bits
        convert = real(bits)
    end function

    pure real function total(b)
        integer, intent(in) :: b(:)
        total = real(sum(b))
    end function
end program
