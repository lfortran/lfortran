program format_104
    character(len=20) :: buf
    write(buf, '(X,A)') "Hello"
    if (buf /= " Hello") error stop
    write(buf, '(A,X,A)') "a", "b"
    if (buf /= "a b") error stop
    print '(X,A)', "Hello world!"
end program
