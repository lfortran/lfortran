! Test writes into substrings inside FileWrite Nodes
program write_13
    implicit none
    character(10):: string = 'ABCDEFGHIJ'
    character(len=5) :: string2

    write(string2(1:),'(a)') "Hello"
    write(string(1:4),'(A)') 'abcd'

    print *, string, string2

    if (string/='abcdEFGHIJ') error stop
    if (string2/='Hello') error stop
end program write_13