subroutine sub(afb)
    real afb(3)
    print *, sum(afb)
    if ( .not. ( (abs(sum(afb) - 9.0) > 1e-8) .or. (abs(sum(afb) - 3.0) > 1e-8) ) ) error stop
end subroutine
