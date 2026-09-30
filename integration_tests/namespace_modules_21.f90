c     Namespace imports in fixed-form source. Blanks are insignificant, so
c     the commas and the double colon keep the statement unambiguous.
      module nsm21mod
      implicit none
      integer :: ival = 3
      contains
      integer function twice(i)
      integer, intent(in) :: i
      twice = 2*i
      end function
      end module

      program namespace_modules_21
      use,namespace::m=>nsm21mod
      use , name space :: nsm21mod
      implicit none
      if (m%ival .ne. 3) error stop
      if (m % twice(m%ival) .ne. 6) error stop
      if (nsm21mod%ival .ne. 3) error stop
      m%ival = m%t wice(5)
      if (nsm21mod % ival .ne. 10) error stop
      print *, m%ival
      end program
