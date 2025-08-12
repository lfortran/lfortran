program string_64

    character(len=1),  parameter :: NUL = achar(int(z'00')) !! Null
    character(len=1),  parameter :: TAB = achar(int(z'09')) !! Horizontal tab
    character(len=1),  parameter :: LF  = achar(int(z'0A')) !! NL line feed, new line
    character(len=1),  parameter :: VT  = achar(int(z'0B')) !! Vertical tab
    character(len=1),  parameter :: FF  = achar(int(z'0C')) !! NP form feed, new page
    character(len=1),  parameter :: CR  = achar(int(z'0D')) !! Carriage return
    character(len=5) :: testMdata(2,2)

    testMdata = reshape( [ character(len=5) :: &
        TAB, &
        "X"//TAB//"Y", &
        "YES", &
        "NO" ], [2,2] )

    if (testMdata(1,1) /= TAB .or. testMdata(1,2) /= "YES") error stop
    if (testMdata(2,1) /= "X"//TAB//"Y" .or. testMdata(2,2) /= "NO") error stop

    print *, len(NUL//TAB//LF//VT//FF//CR)
    if(len(NUL//TAB//LF//VT//FF//CR) /= 6) error stop
 
 end program 
 