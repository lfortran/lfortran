program wasm_if
    implicit none

    logical :: x, y
    x = .true.
    y = .false.

    if ( x ) print *, "x: Single Line If"
    if ( y ) print *, "y: Single Line If"

    if ( x ) then
        print *, "x: Multi Line If-Else: x is True"
    else
        print *, "x: Multi Line If-Else: x is False"
    end if

    if ( y ) then
        print *, "y: Multi Line If-Else: y is True"
    else
        print *, "y: Multi Line If-Else: y is False"
    end if


    ! if else-if else ladder
    if ( x ) then
        print *, "x: If part"
    else if(x) then
        print *, "x: Else-If part"
    end if

    if ( y ) then
        print *, "y: If part"
    else if(x) then
        print *, "x: Else-If part"
    else
        print *, "Else part"
    end if

    if(y) then
        print *, "y: If part"
    else if(y) then
        print *, "y: Else-If part"
    else
        print *, "Else part"
    end if

end program
