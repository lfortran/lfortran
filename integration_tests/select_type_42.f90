! Test: unformatted stream write of array inside select type on class(*)
program select_type_42
    use iso_fortran_env, only: int32
    implicit none

    integer(int32) :: a(3)
    a = [10, 20, 30]
    call check_array(a)

contains

    subroutine check_array(x)
        class(*), intent(in) :: x(:)
        integer :: unit
        integer(int32) :: buf(3)

        select type (x)
        type is (integer(int32))
            open(newunit=unit, file='select_type_42.bin', status='replace', &
                 action='write', access='stream')
            write(unit) x
            close(unit)
        end select

        open(newunit=unit, file='select_type_42.bin', status='old', &
             action='read', access='stream')
        read(unit) buf
        close(unit, status='delete')

        if (buf(1) /= 10) error stop
        if (buf(2) /= 20) error stop
        if (buf(3) /= 30) error stop
        print *, "PASS"
    end subroutine

end program
