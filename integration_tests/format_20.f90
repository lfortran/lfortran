program format_20

    type :: struct
       character(17) :: field
       integer :: int
    end type struct
    
    type(struct) :: array(3)
    character(*), parameter :: title = 'Some title string: '
    character(100) :: res
    character(100) :: expected_res = "Some title string: 1234567890     ABCDEFGHIJ     Abcdefghijklmn"
    array(1) = struct('1234567890',1)
    array(2) = struct('ABCDEFGHIJ',2)
    array(3) = struct('Abcdefghijklmn',3)
    print '(a,3a15)', title, array(:)%field
    write(*,'(a,3a15)') title, array(:)%field
    write(res,'(a,3a15)') title, array(:)%field
    ! The rest of `res` and `expected_res` length are empty spaces (which is fine in this test).
    if(res /= expected_res) error stop    
end program
  