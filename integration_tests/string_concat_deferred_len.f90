program string_concat_deferred_len
implicit none
character(len=:), allocatable :: p, c

p = '././src/fpm_meta.f90'
c = ''

if (p(1:1) /= '.') c = c // p(1:1) // '/'
c = c // p(5:len(p)) // '/'

if (len(c) > 1 .and. c(len(c):) == '/') c = c(:len(c)-1)

if (trim(c) /= 'src/fpm_meta.f90') error stop 1
end program
