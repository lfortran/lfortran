subroutine f()
    real :: var = 0.0
    save :: var
end subroutine

real function g()
    real :: count = 1.0
    save :: count
    g = count
end function
