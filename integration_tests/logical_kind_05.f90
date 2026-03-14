      program logical_kind_04
          implicit none
          integer, parameter :: lp1 = 1, lp2 = 2, lp4 = 4, lp8 = 8

          ! --- Numeric kind suffixes ---
          logical(1) :: n1t, n1f
          logical(2) :: n2t, n2f
          logical(4) :: n4t, n4f
          logical(8) :: n8t, n8f

          ! --- Named-parameter kind suffixes ---
          logical(lp1) :: p1t, p1f
          logical(lp2) :: p2t, p2f
          logical(lp4) :: p4t, p4f
          logical(lp8) :: p8t, p8f

          ! --- Capitalization variants (kind 4) ---
          logical(4) :: cap1, cap2, cap3, cap4, cap5, cap6

          ! ===== Section 1: numeric kind literals =====
          n1t = .true._1
          n1f = .false._1
          n2t = .true._2
          n2f = .false._2
          n4t = .true._4
          n4f = .false._4
          n8t = .true._8
          n8f = .false._8

          ! Check truth values
          if (n1t .neqv. .true.)  error stop
          if (n1f .neqv. .false.) error stop
          if (n2t .neqv. .true.)  error stop
          if (n2f .neqv. .false.) error stop
          if (n4t .neqv. .true.)  error stop
          if (n4f .neqv. .false.) error stop
          if (n8t .neqv. .true.)  error stop
          if (n8f .neqv. .false.) error stop

          ! Check kinds via storage_size (returns size in bits)
          if (storage_size(n1t) /= 8)  error stop
          if (storage_size(n1f) /= 8)  error stop
          if (storage_size(n2t) /= 16) error stop
          if (storage_size(n2f) /= 16) error stop
          if (storage_size(n4t) /= 32) error stop
          if (storage_size(n4f) /= 32) error stop
          if (storage_size(n8t) /= 64) error stop
          if (storage_size(n8f) /= 64) error stop

          ! Check kind() of the literal itself
          if (kind(.true._1)  /= 1) error stop
          if (kind(.false._1) /= 1) error stop
          if (kind(.true._2)  /= 2) error stop
          if (kind(.false._2) /= 2) error stop
          if (kind(.true._4)  /= 4) error stop
          if (kind(.false._4) /= 4) error stop
          if (kind(.true._8)  /= 8) error stop
          if (kind(.false._8) /= 8) error stop

          ! ===== Section 2: named-parameter kind literals =====
          p1t = .true._lp1
          p1f = .false._lp1
          p2t = .true._lp2
          p2f = .false._lp2
          p4t = .true._lp4
          p4f = .false._lp4
          p8t = .true._lp8
          p8f = .false._lp8

          ! Check truth values
          if (p1t .neqv. .true.)  error stop
          if (p1f .neqv. .false.) error stop
          if (p2t .neqv. .true.)  error stop
          if (p2f .neqv. .false.) error stop
          if (p4t .neqv. .true.)  error stop
          if (p4f .neqv. .false.) error stop
          if (p8t .neqv. .true.)  error stop
          if (p8f .neqv. .false.) error stop

          ! Check kinds via storage_size
          if (storage_size(p1t) /= 8)  error stop
          if (storage_size(p1f) /= 8)  error stop
          if (storage_size(p2t) /= 16) error stop
          if (storage_size(p2f) /= 16) error stop
          if (storage_size(p4t) /= 32) error stop
          if (storage_size(p4f) /= 32) error stop
          if (storage_size(p8t) /= 64) error stop
          if (storage_size(p8f) /= 64) error stop

          ! Check kind() of named-parameter literals
          if (kind(.true._lp1)  /= 1) error stop
          if (kind(.false._lp1) /= 1) error stop
          if (kind(.true._lp2)  /= 2) error stop
          if (kind(.false._lp2) /= 2) error stop
          if (kind(.true._lp4)  /= 4) error stop
          if (kind(.false._lp4) /= 4) error stop
          if (kind(.true._lp8)  /= 8) error stop
          if (kind(.false._lp8) /= 8) error stop

          ! ===== Section 3: capitalization variants =====
          cap1 = .true._4
          cap2 = .TRUE._4
          cap3 = .True._4
          cap4 = .false._4
          cap5 = .FALSE._4
          cap6 = .False._4

          if (cap1 .neqv. .true.)  error stop
          if (cap2 .neqv. .true.)  error stop
          if (cap3 .neqv. .true.)  error stop
          if (cap4 .neqv. .false.) error stop
          if (cap5 .neqv. .false.) error stop
          if (cap6 .neqv. .false.) error stop

          ! Also with named parameter kinds and mixed case
          if (.TRUE._lp1  .neqv. .true.)  error stop
          if (.True._lp2  .neqv. .true.)  error stop
          if (.FALSE._lp1 .neqv. .false.) error stop
          if (.False._lp2 .neqv. .false.) error stop
          if (kind(.TRUE._lp8)  /= 8) error stop
          if (kind(.False._lp1) /= 1) error stop

          ! ===== Section 4: cross-kind assignment =====
          n8t = .true._1
          if (n8t .neqv. .true.) error stop
          if (storage_size(n8t) /= 64) error stop

          n1f = .false._8
          if (n1f .neqv. .false.) error stop
          if (storage_size(n1f) /= 8) error stop

          print *, "All logical_kind_04 tests passed."
      end program
