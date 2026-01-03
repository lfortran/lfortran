MODULE array_init_module
    TYPE tp
        INTEGER(4) i
    END TYPE tp
END MODULE array_init_module

PROGRAM t
    USE array_init_module
    TYPE (tp) v1,v2,v3
    v1%i = 1
    v2%i = 2
    v3%i = 3
    CALL s([tp :: v1,v2,v3], 3)
    CALL s([tp ::], 0)

CONTAINS
    SUBROUTINE s(va, sz)
        USE array_init_module
        TYPE(tp) va(:)
        INTEGER sz
        print *, size(va)
        if (size(va) /= sz) error stop
    END SUBROUTINE s
END PROGRAM t