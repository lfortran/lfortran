program expr_11
! Test parantheses in expressions with type casts (#1043)
    real*8 x

    x=(2.0*x+1.0)/(x*(x+1))
end program
