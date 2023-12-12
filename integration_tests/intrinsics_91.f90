program intrinsics_91
    real(4) a
    real(8) b
    print*, tiny(a)
    print*, tiny(b)
    print*, kind(tiny(a))
    print*, kind(tiny(b))
end
