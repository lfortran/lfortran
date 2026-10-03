program print_17
! A list-directed output list that is not empty in the source but expands
! to no items at run time writes an empty record. The leading blank of
! list-directed output under --std=f23 is written only when at least one
! item is written.
implicit none
integer :: a(0), b(3), i, j, u
integer, allocatable :: c(:)
character(len=3) :: s(0)

b = [1, 2, 3]
allocate(c(0))
open(newunit=u, file="print_17.txt", status="replace", form="formatted")
write(u, *) a
write(u, *) (i, i = 1, 0)
write(u, *) a, a
write(u, *) c
write(u, *) b(2:1)
write(u, *) s
write(u, *) ((i, i = 1, j), j = 1, 0)
write(u, '(a)', advance='no') "ab"
write(u, *) a
write(u, *) a, 5
write(u, *) a, "x", s
close(u)

open(newunit=u, file="print_17.txt", status="old", form="formatted")
do i = 1, 7
    call check_record(u, "")
end do
call check_record(u, "ab")
call check_record(u, " 5")
call check_record(u, " x")
close(u, status="delete")

print *, a
print *, (i, i = 1, 0)
print *, c, b(2:1), s
print *, "PASS"

contains

    ! Reads the next record and compares it with `expected`. A record that
    ! holds a single integer is compared after its leading blanks, as the
    ! width of an integer field in list-directed output is processor
    ! dependent; it must still start with a blank.
    subroutine check_record(unit, expected)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: expected
        character(len=40) :: buf
        integer :: n, ios
        buf = "?"
        read(unit, '(a)', advance='no', size=n, iostat=ios) buf
        if (expected == " 5") then
            if (buf(1:1) /= " ") error stop "list must start with a blank"
            if (adjustl(buf(1:n)) /= "5") error stop "wrong item"
        else
            if (n /= len(expected)) error stop "wrong record length"
            if (buf(1:n) /= expected) error stop "wrong record"
        end if
    end subroutine

end program
