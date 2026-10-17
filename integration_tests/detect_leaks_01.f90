! Many allocate/deallocate cycles under --detect-leaks (issue #15352).
! The array size changes every round, so the heap hands out new addresses
! and the leak tracker sees many distinct pointers while only a few are
! live. Their deleted hash-table buckets must be reclaimed; otherwise every
! allocation scans the whole table.
program detect_leaks_01
    implicit none
    type :: box
        integer, allocatable :: v(:)
    end type
    type(box) :: b(500)
    integer :: i, j, n
    integer(8) :: total

    total = 0
    do j = 1, 200
        n = 300 + 4*j
        do i = 1, size(b)
            allocate(b(i)%v(n))
            b(i)%v(1) = i
            b(i)%v(n) = j
        end do
        do i = 1, size(b)
            total = total + b(i)%v(1) + b(i)%v(n)
            deallocate(b(i)%v)
        end do
    end do

    print *, total
    if (total /= 35100000_8) error stop
end program
