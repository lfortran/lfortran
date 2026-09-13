program inquire_iolength_string
   implicit none
   character(len=10) :: tablename
   integer           :: howbig
   tablename='my table'
   
   inquire(iolength=howbig) 'insert into ', tablename
   if (howbig /= 22) error stop
   
   inquire(iolength=howbig) 'insert into ', len(tablename)
   if (howbig /= 16) error stop
   
   inquire(iolength=howbig) 'insert into ', adjustl(tablename)
   if (howbig /= 22) error stop
   
   inquire(iolength=howbig) 'insert into ', tablename//tablename
   if (howbig /= 32) error stop
   
   inquire(iolength=howbig) 'insert into ', trim(tablename)
   if (howbig /= 20) error stop
end program inquire_iolength_string
