module private_reduction
    use iso_fortran_env, only: int16, int32, real32
    implicit none
contains
    pure elemental function byte_value(word, lane) result(q)
        integer(int32), intent(in) :: word
        integer, intent(in) :: lane
        integer(int32) :: q
        if (lane < 0 .or. lane > 3) error stop "invalid byte index"
        q = ibits(word, 8*lane, 8)
    end function

    pure elemental function bf16(value) result(bits)
        real(real32), intent(in) :: value
        integer(int16) :: bits
        integer(int32) :: word, upper, lower
        word = transfer(value, 0_int32)
        upper = ibits(word, 16, 16)
        lower = ibits(word, 0, 16)
        if (ibits(word, 23, 8) == 255 .and. ibits(word, 0, 23) /= 0) then
            upper = ibset(upper, 6)
        else if (lower > 32768 .or. (lower == 32768 .and. btest(upper, 0))) then
            upper = upper + 1
        end if
        bits = int(ibits(upper, 0, 15), int16)
        if (btest(upper, 15)) bits = ibset(bits, 15)
    end function

    pure subroutine project(w, x, y)
        integer(int32), contiguous, intent(in) :: w(:,:)
        real(real32), contiguous, intent(in) :: x(:)
        integer(int16), contiguous, intent(out) :: y(:)
        integer :: n, d
        do concurrent (n = 1:size(y))
            block
                real(real32) :: codes(size(x))
                do concurrent (d = 1:size(x))
                    codes(d) = real(byte_value(w((d-1)/4+1,n),modulo(d-1,4)),real32)
                end do
                y(n) = bf16(sum(codes*x))
            end block
        end do
    end subroutine

    subroutine check(k, outputs, pattern)
        integer, intent(in) :: k, outputs, pattern
        integer(int32) :: w(k/4,outputs)
        real(real32) :: x(k), total
        integer(int16) :: y(outputs)
        integer :: d, n
        do d = 1,k
            x(d) = real(modulo(d*17,31)-15,real32)/32.0_real32
            if (modulo(d,8) == 0) x(d) = -0.0_real32
            if (pattern == 2) then
                x(d) = 0.00390625_real32
                if (modulo(d,4) == 1) x(d) = 65536.0_real32
                if (modulo(d,4) == 3) x(d) = -65536.0_real32
            end if
            if (pattern == 3) x(d) = 0.0_real32
        end do
        if (pattern == 3) then
            x(1) = 262144.0_real32
            x(2) = -262144.0_real32
            x(3) = 1.0_real32
        end if
        do n = 1,outputs
            do d = 1,k/4
                w(d,n) = -8356096_int32
                if (modulo(d+n,2) == 0) w(d,n) = 8356095_int32
            end do
            if (pattern == 3) w(1,n) = 98432_int32
        end do
        call project(w,x,y)
        do n = 1,outputs
            total = 0.0_real32
            do d = 1,k
                total = total + real(byte_value(w((d-1)/4+1,n),modulo(d-1,4)),real32)*x(d)
            end do
            if (y(n) /= bf16(total)) error stop "complete bf16 output mismatch"
            if (pattern == 3 .and. y(n) /= 16256_int16) error stop "ordered fold changed"
        end do
    end subroutine
end module

program gpu_private_reduction_01
    use private_reduction
    implicit none
    call check(64,2,1)
    call check(64,2,2)
    call check(64,2,3)
    call check(512,8,1)
    call check(64,0,1)
    call check(0,2,1)
    print *, "ok"
end program
