module gpu_metal_659_m
    use iso_fortran_env, only: int16, int32, int64, real32
    implicit none
contains
    pure subroutine evaluate(out, blocks)
        integer(int16), contiguous, intent(out) :: out(:, :, :)
        integer, intent(in) :: blocks
        integer :: d_head, token, h
        d_head = size(out, 1)
        do concurrent (token = 1:size(out,3), h = 1:size(out,2))
            block
                real(real32) :: factors(blocks), total
                integer(int16) :: partials(d_head,blocks)
                integer :: d
                partials = 16256_int16
                factors = 1.0_real32
                total = 1.0_real32
                do concurrent (d = 1:d_head)
                    out(d,h,token) = real32_to_bf16_bits( &
                        sum(bf16_bits_to_real32(partials(d,:))*factors)/total)
                end do
            end block
        end do
    end subroutine

    pure subroutine evaluate_bounds(out, blocks, lower)
        integer(int16), contiguous, intent(out) :: out(:, :, :)
        integer(int64), intent(in) :: blocks, lower
        integer :: d_head, token, h
        d_head = size(out, 1)
        do concurrent (token = 1:size(out,3), h = 1:size(out,2))
            block
                real(real32) :: factors(blocks), total
                integer(int16) :: partials(-1:d_head-2,lower:lower+blocks-1)
                integer :: d
                partials = 16256_int16
                factors = 1.0_real32
                total = 1.0_real32
                do concurrent (d = 1:d_head)
                    out(d,h,token) = real32_to_bf16_bits( &
                        sum(bf16_bits_to_real32(partials(d-2,:))*factors)/total)
                end do
            end block
        end do
    end subroutine

    pure elemental function bf16_bits_to_fp32_bits(bits) result(word)
        integer(int16), intent(in) :: bits
        integer(int32) :: word
        word = shiftl(int(ibits(bits, 0, 15), int32), 16)
        if (btest(bits, 15)) word = ibset(word, 31)
    end function

    pure elemental function fp32_bits_to_bf16_bits(word) result(bits)
        integer(int32), intent(in) :: word
        integer(int16) :: bits
        integer(int32) :: upper, lower
        upper = ibits(word, 16, 16)
        lower = ibits(word, 0, 16)
        if (ibits(word, 23, 8) == 255_int32 .and. ibits(word, 0, 23) /= 0_int32) then
            upper = ibset(upper, 6)
        else if (lower > 32768_int32 .or. (lower == 32768_int32 .and. btest(upper, 0))) then
            upper = upper + 1_int32
        end if
        bits = int(ibits(upper, 0, 15), int16)
        if (btest(upper, 15)) bits = ibset(bits, 15)
    end function

    pure elemental function bf16_bits_to_real32(bits) result(value)
        integer(int16), intent(in) :: bits
        real(real32) :: value
        value = transfer(bf16_bits_to_fp32_bits(bits), 0.0_real32)
    end function

    pure elemental function real32_to_bf16_bits(value) result(bits)
        real(real32), intent(in) :: value
        integer(int16) :: bits
        bits = fp32_bits_to_bf16_bits(transfer(value, 0_int32))
    end function
end module

program gpu_metal_659
    use gpu_metal_659_m
    implicit none
    integer(int64) :: blocks, lower
    integer(int16) :: out(3,2,2)
    integer :: d, h, token, test, bounds_case
    do test = 1, 3
        blocks = int(command_argument_count(),int64) + 4_int64
        if (test == 2) blocks = 0
        if (test == 3) blocks = 1
        lower = -3_int64
        if (test == 3) lower = 2_int64
        do bounds_case = 1, 2
            if (bounds_case == 1) then
                call evaluate(out, int(blocks))
            else
                call evaluate_bounds(out, blocks, lower)
            end if
            do token = 1, size(out,3)
                do h = 1, size(out,2)
                    do d = 1, size(out,1)
                        if (abs(bf16_bits_to_real32(out(d,h,token))-real(blocks,real32)) > 1.0e-5_real32) error stop
                    end do
                end do
            end do
        end do
    end do
    print *, "runtime block row gather: ok"
end program
