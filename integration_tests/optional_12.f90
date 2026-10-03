subroutine sr_optional_12_sub(arr)
    integer, dimension(..), optional, intent(in) :: arr
    integer, allocatable, dimension(:) :: out

    allocate(out(1))
    if (present(arr)) then
        select rank(arr)
        rank(0)
            out = arr
        rank(1)
            out = arr(1)
        end select
    end if
    if (out(1) /= 5) error stop
end subroutine

program optional_12
    implicit none
    integer, allocatable :: a(:)

    interface
        subroutine sr_optional_12_sub(arr)
            integer, dimension(..), optional, intent(in) :: arr
        end subroutine
    end interface

    allocate(a(1))
    a(1) = 5
    call sr_optional_12_sub(a)
end program
