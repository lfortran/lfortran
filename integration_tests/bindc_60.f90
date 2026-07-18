module bindc_60_mod
   ! A length-1, default-kind character dummy in a BIND(C) interface is
   ! interoperable with a C `char` and must be passed as `char*`. Regression
   ! test: `character(len=c_char)`, `character(len=1)` and bare `character`
   ! used to be passed with the wrong physical type (a string descriptor),
   ! so the C callee received a garbage/off-by-one byte. Only the explicit
   ! `character(kind=c_char)` form worked. All four must now agree.
   use iso_c_binding, only: c_int, c_char
   implicit none
   interface
      function cput_lenc(op) bind(c, name="echo_byte_lenc") result(status)
         import :: c_int, c_char
         character(len=c_char), intent(in) :: op
         integer(c_int) :: status
      end function cput_lenc

      function cput_len1(op) bind(c, name="echo_byte_len1") result(status)
         import :: c_int
         character(len=1), intent(in) :: op
         integer(c_int) :: status
      end function cput_len1

      function cput_bare(op) bind(c, name="echo_byte_bare") result(status)
         import :: c_int
         character, intent(in) :: op
         integer(c_int) :: status
      end function cput_bare

      function cput_kindc(op) bind(c, name="echo_byte_kindc") result(status)
         import :: c_int, c_char
         character(kind=c_char), intent(in) :: op
         integer(c_int) :: status
      end function cput_kindc
   end interface
end module bindc_60_mod

program bindc_60
   use iso_c_binding, only: c_int
   use bindc_60_mod
   implicit none
   character(len=1)   :: chval
   integer(c_int)     :: received
   integer, parameter :: sent = 65      ! 'A'

   chval = char(sent)

   received = cput_lenc(chval)
   print *, "len=c_char  : received =", received
   if (received /= sent) error stop 1

   received = cput_len1(chval)
   print *, "len=1       : received =", received
   if (received /= sent) error stop 2

   received = cput_bare(chval)
   print *, "bare char   : received =", received
   if (received /= sent) error stop 3

   received = cput_kindc(chval)
   print *, "kind=c_char : received =", received
   if (received /= sent) error stop 4

   print *, "OK"
end program bindc_60
