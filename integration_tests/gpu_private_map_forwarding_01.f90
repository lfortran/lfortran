module private_map_forwarding
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

    ! `codes` is decoded once and read element by element by the fold.
    ! `scaled` is decoded from `codes` only when `direct` is false, and read
    ! in the branch that `direct` selects. Neither map needs storage of its
    ! own once it is forwarded into its reads.
    pure subroutine project(w, x, y, direct)
        integer(int32), contiguous, intent(in) :: w(:,:)
        real(real32), contiguous, intent(in) :: x(:)
        integer(int16), contiguous, intent(out) :: y(:)
        logical, intent(in) :: direct
        integer :: n, d
        do concurrent (n = 1:size(y))
            block
                real(real32) :: codes(size(x))
                real(real32) :: scaled(size(x))
                do concurrent (d = 1:size(x))
                    codes(d) = real(byte_value(w((d-1)/4+1,n),modulo(d-1,4)),real32)
                    if (.not. direct) scaled(d) = codes(d)*0.5_real32
                end do
                if (direct) then
                    y(n) = bf16(sum(codes*x))
                else
                    y(n) = bf16(sum(scaled*x))
                end if
            end block
        end do
    end subroutine

    subroutine check(k, outputs, direct)
        integer, intent(in) :: k, outputs
        logical, intent(in) :: direct
        integer(int32) :: w(k/4,outputs)
        real(real32) :: x(k), total, scale
        integer(int16) :: y(outputs)
        integer :: d, n
        do d = 1,k
            x(d) = real(modulo(d*17,31)-15,real32)/32.0_real32
        end do
        do n = 1,outputs
            do d = 1,k/4
                w(d,n) = -8356096_int32
                if (modulo(d+n,2) == 0) w(d,n) = 8356095_int32
            end do
        end do
        call project(w,x,y,direct)
        scale = 1.0_real32
        if (.not. direct) scale = 0.5_real32
        do n = 1,outputs
            total = 0.0_real32
            do d = 1,k
                total = total + (scale*real( &
                    byte_value(w((d-1)/4+1,n),modulo(d-1,4)),real32))*x(d)
            end do
            if (y(n) /= bf16(total)) error stop "bf16 output mismatch"
        end do
    end subroutine
end module

program gpu_private_map_forwarding_01
    use private_map_forwarding
    implicit none
    call check(64,2,.true.)
    call check(64,2,.false.)
    call check(512,8,.true.)
    call check(512,8,.false.)
    call check(0,2,.true.)
    print *, "ok"
end program
