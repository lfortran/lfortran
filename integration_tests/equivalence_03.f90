program equivalence_03
    implicit none
    DOUBLE PRECISION DMACH(5)
    INTEGER*4 SMALL(2)
    INTEGER*4 LARGE(2)
    INTEGER*4 RIGHT(2)
    INTEGER*4 DIVER(2)
    INTEGER*4 LOG10(2)
    EQUIVALENCE (DMACH(1),SMALL(1))
    EQUIVALENCE (DMACH(2),LARGE(1))
    EQUIVALENCE (DMACH(3),RIGHT(1))
    EQUIVALENCE (DMACH(4),DIVER(1))
    EQUIVALENCE (DMACH(5),LOG10(1))
    dmach(2) = 5.6_8
    if(large(1) - 1717986918 > 1e-15) error stop
    if(large(2) - 1075209830 > 1e-15) error stop
    dmach(2) = 5.7_8
    if(large(1) - 858993459 > 1e-15) error stop
    if(large(2) - 1075236044 > 1e-15) error stop
end