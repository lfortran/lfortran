program gpu_metal_622
    ! Mixed-width integer(1)/integer(2) scalar kernel arguments, a derived
    ! type with narrow members, and device functions with integer(1) and
    ! integer(2) results and arguments, in loops offloaded to the GPU.
    use iso_fortran_env, only: int8, int16, int32
    implicit none
    type :: narrow_t
        integer(int8) :: a
        integer(int16) :: b
        logical(1) :: f
        integer(int8) :: c
        real :: x
        integer(int16) :: d
    end type
    integer(int8) :: s1, s3, s7
    integer(int16) :: s2, s5
    integer(int32) :: s4
    real :: s6
    logical(1) :: l1
    integer :: r(4), q(2), i
    integer(int8) :: b(4)
    integer(int16) :: h(4)
    type(narrow_t) :: s(4)

    s1 = -3_int8; s2 = -1234_int16; s3 = 77_int8; s4 = 100000
    s5 = 31000_int16; s6 = 2.5; l1 = .true.; s7 = -127_int8 - 1_int8
    do concurrent (i = 1:4)
        r(i) = s1 + s2 + s3 + s4 + s5 + int(s6) + merge(1, 0, l1) + s7 + i
    end do
    if (any(r /= 129715 + [1, 2, 3, 4])) error stop 1

    do i = 1, 4
        s(i) = narrow_t(int(i, int8), int(-100*i, int16), .false., 0_int8, &
            0.0, 0_int16)
    end do
    do concurrent (i = 1:4)
        s(i)%c = s(i)%a * 2_int8
        s(i)%d = s(i)%b - 1_int16
        s(i)%f = s(i)%b < -150_int16
        s(i)%x = real(s(i)%a) + 0.5
    end do
    do i = 1, 4
        if (s(i)%c /= 2*i .or. s(i)%d /= -100*i - 1) error stop 2
        if (s(i)%f .neqv. (i >= 2)) error stop 3
        if (s(i)%a /= i .or. s(i)%b /= -100*i) error stop 4
        if (abs(s(i)%x - (i + 0.5)) > 1e-6) error stop 5
    end do

    do concurrent (i = 1:2)
        q(i) = int(small(i), 4)
    end do
    if (any(q /= [2, 3])) error stop 6

    do concurrent (i = 1:4)
        b(i) = neg8(int(i, int8))
        h(i) = 0_int16
        call bump(h(i), int(i, int8))
    end do
    if (any(b /= [-1_int8, -2_int8, -3_int8, -4_int8])) error stop 7
    if (any(h /= [10_int16, 20_int16, 30_int16, 40_int16])) error stop 8
    print *, r, q, b, h
contains
    pure integer(2) function small(i) result(r)
        integer, intent(in) :: i
        r = int(i + 1, 2)
    end function

    pure integer(int8) function neg8(x) result(r)
        integer(int8), intent(in) :: x
        r = -x
    end function

    pure subroutine bump(y, k)
        integer(int16), intent(inout) :: y
        integer(int8), value :: k
        y = y + 10_int16 * int(k, int16)
    end subroutine
end program
