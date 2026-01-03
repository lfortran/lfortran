program intrinsics_397
    implicit none
    integer, parameter :: lk1 = selected_logical_kind(1)
    integer, parameter :: lk8 = selected_logical_kind(8)
    integer, parameter :: lk16 = selected_logical_kind(16)
    integer, parameter :: lk32 = selected_logical_kind(32)
    integer, parameter :: lk64 = selected_logical_kind(64)
    integer, parameter :: lk128 = selected_logical_kind(128)
    integer, parameter :: lk256 = selected_logical_kind(256)
    
    ! Test with variables
    integer :: bits
    integer :: kind_result
    
    ! Check parameter evaluations
    if (lk1 /= 1) error stop
    if (lk8 /= 1) error stop
    if (lk16 /= 2) error stop
    if (lk32 /= 4) error stop
    if (lk64 /= 8) error stop
    if (lk128 /= 16) error stop
    if (lk256 /= -1) error stop  ! Not supported
    
    ! Test with runtime values
    bits = 4
    kind_result = selected_logical_kind(bits)
    if (kind_result /= 1) error stop
    
    bits = 16
    kind_result = selected_logical_kind(bits)
    if (kind_result /= 2) error stop
    
    bits = 24
    kind_result = selected_logical_kind(bits)
    if (kind_result /= 4) error stop
    
    bits = 48
    kind_result = selected_logical_kind(bits)
    if (kind_result /= 8) error stop
    
    bits = 100
    kind_result = selected_logical_kind(bits)
    if (kind_result /= 16) error stop
    
    bits = 200
    kind_result = selected_logical_kind(bits)
    if (kind_result /= -1) error stop  ! Not supported
    
end program intrinsics_397
