program main
    implicit none
    character(len=27) :: warn1
    character(len=7) :: warn2
    write(warn1, '(a,1x,a,1x,a,1x,a)') 'KEYWORD', 'SHORT', 'PRESENT', 'VALUE'
    print*, "!", warn1, "!"
    if ( .not. (warn1 /= ' KEYWORD SHORT PRESENT VALUE' .or. warn1 /= 'KEYWORD SHORT PRESENT VALUE' ) ) error stop
    write(warn2, '(i1,1x,a)') 7, 'SHORT'
    print*, "!", warn2, "!"
    if ( warn2 /= '7 SHORT' ) error stop
end program
