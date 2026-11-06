module assumed_rank_26_m
    implicit none
contains
    integer function alloc_size(x) result(n)
        class(*), allocatable, intent(in) :: x(..)

        n = -1
        if (.not. allocated(x)) return

        select rank (x)
        rank (0)
            n = 1
        rank default
            n = size(x)
        end select
    end function alloc_size
end module assumed_rank_26_m

program assumed_rank_26
    use assumed_rank_26_m, only: alloc_size
    implicit none
    class(*), allocatable :: scalar
    class(*), allocatable :: vector(:)
    class(*), allocatable :: matrix(:,:)

    call check(alloc_size(scalar), -1, 'unallocated scalar')
    call check(alloc_size(vector), -1, 'unallocated vector')
    call check(alloc_size(matrix), -1, 'unallocated matrix')

    allocate(scalar, source=42)
    call check(alloc_size(scalar), 1, 'allocated scalar')

    allocate(vector, source=[1.0, 2.0, 3.0])
    call check(alloc_size(vector), 3, 'rank-1 array')
    deallocate(vector)

    allocate(vector, source=[real ::])
    call check(alloc_size(vector), 0, 'zero-sized array')

    allocate(matrix, source=reshape([1, 2, 3, 4, 5, 6], [2, 3]))
    call check(alloc_size(matrix), 6, 'rank-2 array')

contains

    subroutine check(actual, expected, description)
        integer, intent(in) :: actual
        integer, intent(in) :: expected
        character(*), intent(in) :: description

        if (actual /= expected) then
            print '(a,": got ",i0,", expected ",i0)', description, actual, expected
            error stop 'alloc_size test failed'
        end if
    end subroutine check

end program assumed_rank_26
