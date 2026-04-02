program select_rank_27
    use iso_fortran_env, only: real32
    implicit none
    
    integer :: ps(3)
    ps = [2, 3, 4]
    call foo(ps)

contains
    subroutine foo(pool_size)
        integer, dimension(..), optional, intent(in) :: pool_size
        integer, dimension(3) :: pool_size_
        
        pool_size_ = 2
        if (present(pool_size)) then
            select rank(pool_size)
            rank(0)
                pool_size_ = pool_size
            rank(1)
                pool_size_(1) = pool_size(1)
                if (size(pool_size, dim=1) == 1) then
                    pool_size_(2:) = pool_size(1)
                elseif (size(pool_size, dim=1) == 3) then
                    pool_size_(2:) = pool_size(2:)
                end if
            end select
        end if
        
        if (pool_size_(1) /= 2) error stop
        if (pool_size_(2) /= 3) error stop
        if (pool_size_(3) /= 4) error stop
        print *, "PASS"
    end subroutine
end program select_rank_27
