MODULE lookup_name3_module
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ec
CONTAINS
SUBROUTINE sn_expcoeff( ndimen )
INTEGER, INTENT(IN) :: ndimen
ec(:,1,5) = 1.0
END SUBROUTINE sn_expcoeff
END MODULE lookup_name3_module
