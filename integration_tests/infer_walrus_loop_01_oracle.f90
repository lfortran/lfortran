program infer_walrus_loop_01_oracle
    implicit none
    integer :: calls = 0, total = 0
    integer :: i, j, l
    integer(8) :: k, q
    real :: tmp, values(10), arr(10)
    integer :: nested(4), triplets(9)
    integer(8) :: descending(3)

    do i = 1, 10
        tmp = real(i)
        values(i) = tmp
    end do
    if (tmp /= 10.0) error stop 1
    i = 42
    arr = [(real(i), i = 1, 10)]
    if (any(arr /= values)) error stop 2
    if (i /= 42) error stop 3

    reverse: do k = first(calls), 1, -2
        total = total + int(k)
        if (kind(k) /= 8) error stop 4
    end do reverse
    if (calls /= 1 .or. total /= 9) error stop 5

    block
        integer :: i, subtotal
        subtotal = 0
        do i = 3, 1, -1
            subtotal = subtotal + i
        end do
        if (subtotal /= 6) error stop 6
    end block
    if (i /= 42 .or. total /= 9) error stop 7

    nested = [((j + 10*l, j = 1, 2), l = 1, 2)]
    if (any(nested /= [11, 12, 21, 22])) error stop 8
    triplets = [(j, j+1, j+2, j = 1, 5, 2)]
    if (any(triplets /= [1,2,3,3,4,5,5,6,7])) error stop 9
    descending = [(q, q = 5_8, 1_8, -2_8)]
    if (kind(descending) /= 8) error stop 10
    if (any(descending /= [5_8,3_8,1_8])) error stop 11
    j = 77
    l = 88
    if (j /= 77 .or. l /= 88) error stop 12
contains
    function first(count) result(start)
        integer, intent(inout) :: count
        integer(8) :: start
        count = count + 1
        start = 5_8
    end function
end program
