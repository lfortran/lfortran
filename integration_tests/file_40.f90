! Test that closing a file does not corrupt pre-connected unit properties.
! Bug: remove_from_unit_to_file() was not copying all struct fields when
! shifting entries, causing unit 6 (stdout) to inherit wrong access flags
! from subsequent entries after any file close operation.
program file_40
    implicit none
    integer :: u

    ! Open and close a file - this triggers remove_from_unit_to_file
    open(newunit=u, file='file_40_test.txt', status='replace')
    write(u, *) 'test'
    close(u, status='delete')

    ! Now write to stdout (unit 6) - should still work
    ! Before fix: "Runtime Error: Write operation not allowed on unit 6"
    write(6, *) 'PASS'
end program
