      MODULE fixed_form_module_01_mod
      IMPLICIT NONE
      PRIVATE
      INTEGER :: X = 42
      PUBLIC :: X
      END MODULE fixed_form_module_01_mod

      PROGRAM fixed_form_module_01
      USE fixed_form_module_01_mod
      IF (X /= 42) ERROR STOP 1
      END PROGRAM fixed_form_module_01
