module lookup_name4_time_module_xx
  IMPLICIT NONE
  PUBLIC
  REAL :: tparset_ = 0.0
  REAL :: wtime_ = 0.0
END module lookup_name4_time_module_xx

module lookup_name4_plib_module_xx
use lookup_name4_time_module_xx, only: wtime_
PUBLIC
INTEGER :: iproc = 0
INTEGER :: root = 0
INTEGER :: pinit = 0
INTEGER :: pcomm_set = 0
end module

PROGRAM lookup_name4
  USE lookup_name4_time_module_xx, ONLY: tparset_

  USE lookup_name4_plib_module_xx, ONLY: pinit,         &
    pcomm_set
  tparset_ = tparset_ + 1.0
END PROGRAM lookup_name4
