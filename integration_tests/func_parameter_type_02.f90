SUBROUTINE sub(f)
    EXTERNAL f
    H=HINIT853(f)
    RETURN
END

REAL FUNCTION HINIT853(f)
    HINIT853 = f(3.0)
END 

Real function f(x)
    real :: x
    f = x
end function

program main 
    EXTERNAL f
    call sub(f)
end program