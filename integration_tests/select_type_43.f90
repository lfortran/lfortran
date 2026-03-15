! Test: unformatted stream write of logical array inside select type
program select_type_43
    use iso_fortran_env, only: int8
    implicit none

    logical(int8) :: arr(2)
    arr = .true.
    call write_poly(arr)

contains

    subroutine write_poly(a)
        class(*), intent(in) :: a(:)
        integer :: unit
        integer(int8) :: bytes(2)

        select type (a)
        type is (logical(int8))
            open(newunit=unit, file='select_type_43.bin', status='replace', &
                 action='write', access='stream')
            write(unit) a
            close(unit)
        end select

        open(newunit=unit, file='select_type_43.bin', status='old', &
             action='read', access='stream')
        read(unit) bytes
        close(unit, status='delete')

        if (bytes(1) /= 1) error stop
        if (bytes(2) /= 1) error stop
        print *, "PASS"
    end subroutine

end program
