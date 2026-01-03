subroutine dqc25s(f)
    double precision dlog,f
    external f
    call dqk15w(f)
    print *, f(hlgth+centr)
    print *, dlog(2.0d0)
end subroutine
