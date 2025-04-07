program integerStringInput_int64
    use iso_fortran_env, only: int64     ! Import int64 kind (as this was left)
    implicit none
    integer(int64) :: x                   ! Declare x as int64 explicitly
    integer :: ios

    open(unit=10, file="invalidInput_integer.txt", status="old")

    do
        read(10, *, iostat=ios) x
        if (ios /= 0) then
            cycle
        end if
        print *, "Read int64 integer:", x
    end do

    close(10)   
end program integerStringInput_int64