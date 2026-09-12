program llvm_target_options_01
    implicit none
    integer :: i
    i = 2
    if (i * i /= 4) error stop
end program llvm_target_options_01
