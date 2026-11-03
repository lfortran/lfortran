module gpu_identifier_helpers
    implicit none
    type :: half
        integer :: kernel, kernel_gpu, half
    end type
    type :: class
        integer :: float, float_gpu, float_gpu1, and
    end type
contains
    pure integer function cpp_keywords(float, float_gpu) result(int)
        integer, intent(in) :: float, float_gpu
        integer :: int_gpu, int_gpu1
        type(class) :: const
        const%float = float
        const%float_gpu = float_gpu
        const%float_gpu1 = 3 * float
        const%and = 4 * float
        int_gpu = const%float + const%float_gpu
        int_gpu1 = const%float_gpu1 + const%and
        block
            integer :: and, and_gpu, and_gpu1
            and = 5 * float
            and_gpu = 6 * float
            and_gpu1 = 7 * float
            int = int_gpu + int_gpu1 + and + and_gpu + and_gpu1
        end block
    end function

    pure integer function kernel(half, half_gpu) result(device)
        integer, intent(in) :: half, half_gpu
        integer :: thread
        device = half_gpu
        do thread = 1, 2
            block
                integer :: threadgroup
                threadgroup = half + thread
                device = device + threadgroup
            end block
        end do
    end function

    pure integer function evaluate(value) result(constant)
        integer, intent(in) :: value
        type(half) :: thread, thread_gpu
        thread%kernel = value
        thread%kernel_gpu = 2 * value
        thread%half = 3 * value
        thread_gpu%kernel = 4 * value
        thread_gpu%kernel_gpu = 5 * value
        thread_gpu%half = 6 * value
        constant = kernel(thread%kernel + thread%kernel_gpu + thread%half, &
            thread_gpu%kernel + thread_gpu%kernel_gpu + thread_gpu%half)
    end function
end module

program gpu_identifiers_01
    use iso_fortran_env, only: int8, int16, int64
    use gpu_identifier_helpers, only: evaluate, cpp_keywords
    implicit none
    integer :: i, trial, kernel, kernel_gpu, kernel_gpu1
    integer(int8) :: half
    integer(int16) :: half_gpu
    integer(int64) :: thread
    integer(int64) :: device(4), expected
    integer :: a(4), input(4), host(4)

    do trial = 1, 2
        kernel = 5 * trial
        kernel_gpu = 7 * trial
        kernel_gpu1 = 11 * trial
        half = int(-3 * trial, int8)
        half_gpu = int(13 * trial, int16)
        thread = 4294967297_int64 * int(trial, int64)
        do concurrent (i = 1:4)
            device(i) = thread + int(kernel + kernel_gpu + kernel_gpu1, int64) &
                + int(half, int64) + int(half_gpu, int64) + int(i, int64)
        end do
        do i = 1, 4
            expected = 4294967330_int64 * int(trial, int64) + int(i, int64)
            if (device(i) /= expected) error stop 1
        end do
    end do
    do i = 1, 4
        input(i) = i
        host(i) = evaluate(input(i))
    end do
    do concurrent (i = 1:4)
        a(i) = evaluate(input(i))
    end do
    do i = 1, 4
        if (a(i) /= 27 * i + 3) error stop 2
        if (host(i) /= 27 * i + 3) error stop 3
    end do
    do i = 1, 4
        host(i) = cpp_keywords(input(i), 2 * input(i))
    end do
    do concurrent (i = 1:4)
        a(i) = cpp_keywords(input(i), 2 * input(i))
    end do
    do i = 1, 4
        if (a(i) /= 28 * i) error stop 4
        if (host(i) /= 28 * i) error stop 5
    end do
    print *, "GPU identifiers passed"
end program
