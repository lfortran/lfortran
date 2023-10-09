program test_equivalence
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
    print *, large
    dmach(2) = 5.7_8
    print *, large
end


! when body visitor visits  EQUIVALENCE (DMACH(1),SMALL(1)), 
! - Step 1 - make dmach(1) equivalent to pointer of small(1) -> dmach(1) -> small(1). We don't have small pointer yet, hence INTEGER*4, pointer :: SMALL(:)