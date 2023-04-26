program fpm_environment
implicit none

character(len=5) :: quote
quote = merge('"', "'", .true.)

end program
