program wide_char_alloc_01
    ! Test with wide character (kind=2) arrays
    ! to reproduce dominance violation with i16* type
    use iso_fortran_env, only: int16
    implicit none
    character(kind=2, len=:), allocatable :: arr(:)
    character(kind=2, len=100) :: buffer
    integer :: i

    arr = [character(kind=2, len=5) :: 2_"abc", 2_"def", 2_"ghi"]

    do i = 1, 2
        if (i == 1) then
            buffer = arr(1)
        end if
    end do

    print *, "PASS"
end program
