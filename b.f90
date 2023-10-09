program test_equivalence
    use iso_c_binding, only: c_loc, c_f_pointer
    implicit none
    DOUBLE PRECISION, target :: DMACH(5)
    INTEGER*4, pointer :: SMALL(:)
    INTEGER*4, pointer :: LARGE(:)
    INTEGER*4, pointer :: RIGHT(:)
    INTEGER*4, pointer :: DIVER(:)
    INTEGER*4, pointer :: LOG10(:)
    call c_f_pointer(c_loc(dmach(1)), small, [2])
    call c_f_pointer(c_loc(dmach(2)), large, [2])
    call c_f_pointer(c_loc(dmach(3)), right, [2])
    call c_f_pointer(c_loc(dmach(4)), diver, [2])
    call c_f_pointer(c_loc(dmach(5)), log10, [2])
    dmach(2) = 5.6_8
    print *, large
    dmach(2) = 5.7_8
    print *, large
    end