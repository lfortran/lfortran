module private_map_storage
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

    pure elemental function unbf16(bits) result(value)
        integer(int16), intent(in) :: bits
        real(real32) :: value
        integer(int32) :: word
        word = shiftl(int(ibits(bits,0,15),int32),16)
        if (btest(bits,15)) word = ibset(word,31)
        value = transfer(word,0.0_real32)
    end function

    pure elemental function rounded(value) result(r)
        real(real32), intent(in) :: value
        real(real32) :: r
        r = unbf16(bf16(value))
    end function

    pure subroutine project(w,x,scales,offsets,y,vector,parts,bias)
        integer(int32), contiguous, intent(in) :: w(:,:)
        real(real32), contiguous, intent(in) :: x(:,:), scales(:,:), offsets(:,:)
        integer(int16), contiguous, intent(out) :: y(:,:)
        logical, intent(in) :: vector
        integer, intent(in) :: parts
        integer(int16), contiguous, optional, intent(in) :: bias(:)
        integer :: width, groups, partition
        width = size(x,1)
        groups = width/8
        partition = width/parts
        do concurrent (integer :: n=1:size(y,1))
            block
                real(real32) :: codes(width), weights(width)
                do concurrent (integer :: d=1:width)
                    codes(d) = real(byte_value(w((d-1)/4+1,n),modulo(d-1,4)),real32)
                    if (.not. vector) weights(d) = rounded( &
                        codes(d)*scales((d-1)/8+1,n)+offsets((d-1)/8+1,n))
                end do
                do concurrent (integer :: m=1:size(x,2))
                    block
                        real(real32) :: terms(groups), total
                        integer(int16) :: partial_values(parts)
                        if (vector) then
                            do concurrent (integer :: g=1:groups)
                                terms(g) = scales(g,n)*sum(codes((g-1)*8+1:g*8)*x((g-1)*8+1:g*8,m)) + &
                                    offsets(g,n)*sum(x((g-1)*8+1:g*8,m))
                            end do
                            total = sum(terms)
                        else if (parts == 1) then
                            total = sum(weights*x(:,m))
                        else
                            do concurrent (integer :: p=1:parts)
                                partial_values(p) = bf16(sum( &
                                    weights((p-1)*partition+1:p*partition)*x((p-1)*partition+1:p*partition,m)))
                            end do
                            total = sum(unbf16(partial_values))
                        end if
                        total = rounded(total)
                        if (present(bias)) total = total+unbf16(bias(n))
                        y(n,m) = bf16(total)
                    end block
                end do
            end block
        end do
    end subroutine

    subroutine check(width,rows,vector,parts,with_bias)
        integer, intent(in) :: width, rows, parts
        logical, intent(in) :: vector, with_bias
        integer(int32) :: w((width+3)/4,2)
        real(real32) :: x(width,rows), scales((width+7)/8,2), offsets((width+7)/8,2)
        integer(int16) :: y(2,rows), bias(2), partial_values(parts)
        real(real32) :: codes(width), weights(width), total, subtotal, xsum
        integer :: n,m,d,g,p,partition
        do n=1,2
            bias(n) = bf16(real(n,real32)/32.0_real32)
            do d=1,(width+3)/4
                w(d,n) = -8356096_int32
                if (modulo(d+n,2) == 0) w(d,n) = 8356095_int32
            end do
            do g=1,(width+7)/8
                scales(g,n) = real(g+n,real32)/64.0_real32
                offsets(g,n) = real(g-n,real32)/16.0_real32
            end do
        end do
        do m=1,rows
            do d=1,width
                x(d,m) = real(modulo(d*17+m,31)-15,real32)/32.0_real32
            end do
        end do
        if (with_bias) then
            call project(w,x,scales,offsets,y,vector,parts,bias)
        else
            call project(w,x,scales,offsets,y,vector,parts)
        end if
        partition = width/parts
        do n=1,2
            do d=1,width
                codes(d) = real(byte_value(w((d-1)/4+1,n),modulo(d-1,4)),real32)
                weights(d) = rounded(codes(d)*scales((d-1)/8+1,n)+offsets((d-1)/8+1,n))
            end do
            do m=1,rows
                total = 0.0_real32
                if (vector) then
                    do g=1,width/8
                        subtotal = 0.0_real32
                        xsum = 0.0_real32
                        do d=(g-1)*8+1,g*8
                            subtotal = subtotal+codes(d)*x(d,m)
                            xsum = xsum+x(d,m)
                        end do
                        total = total+(scales(g,n)*subtotal+offsets(g,n)*xsum)
                    end do
                else if (parts == 1) then
                    do d=1,width
                        total = total+weights(d)*x(d,m)
                    end do
                else
                    do p=1,parts
                        subtotal = 0.0_real32
                        do d=(p-1)*partition+1,p*partition
                            subtotal = subtotal+weights(d)*x(d,m)
                        end do
                        partial_values(p) = bf16(subtotal)
                    end do
                    do p=1,parts
                        total = total+unbf16(partial_values(p))
                    end do
                end if
                total = rounded(total)
                if (with_bias) total = total+unbf16(bias(n))
                if (y(n,m) /= bf16(total)) error stop "complete bf16 output mismatch"
            end do
        end do
    end subroutine
end module

program gpu_private_map_storage_01
    use private_map_storage
    implicit none
    call check(32,1,.true.,1,.false.)
    call check(32,3,.true.,1,.true.)
    call check(32,1,.false.,1,.true.)
    call check(32,3,.false.,4,.false.)
    call check(34,1,.true.,1,.true.)
    call check(34,3,.false.,4,.true.)
    call check(32,0,.true.,1,.false.)
    call check(0,1,.false.,1,.false.)
    print *, "ok"
end program
