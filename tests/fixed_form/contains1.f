C Program to show 23 ways Fortran uses the asterisk. Comments at ends of 
* lines mark uses of *, as here:    ! Comment line, * character in it.  (1,2)
C This program is valid Fortran 2018, but its fixed source form, alternate
C return, and declaration beginning character* are all obsolescent. Class(*) 
C was new in f2003, unlimited format in F2008, and type(*) and rank(*) in
C F2018. List-directed internal read, use of lower case, and comments that
C begin with ! are its only other features that were not valid Fortran 77.
      program test23stars
      character:: b,c,d='?'
      integer j,k,n(2)
      data j,k/2*0/                 ! repeated value in data            (3)
      call input(n,'2*4',b,c,d,666)! repeated value in list-directed input, 
C                                   ! alternate return                  (4,5)
      write(*,'(2i2,1x,a)') n(1)*j, ! default output unit ! multiply    (6,7)
     *      n(2)**k,b               ! continuation line ! exponentiate  (8,9)
      stop
 666  print '(a)', ' * error in n'  ! in non-comment character string   (10)
      print '(*(i4))', n            ! unlimited format item             (11)

      contains

      subroutine input(n,a,b,c,d,r) ! dummy alternate return argument   (12)
      integer,intent(out)::n(*)     ! assumed-size array                (13)
      character,intent(in)::a*(*)   ! length selector ! assumed length
C                                        following variable name        (14,15)
      character*(*),intent(out)::b  ! length selector ! assumed length
C                                        following character keyword    (16,17)
      type(*),intent(in)::c         ! assumed-type                      (18)
      character(1),intent(in)::d(..)
      integer i
      character ch
      class(*),allocatable:: e(:)   ! polymorphic entity                (19)
      select rank (d)
      rank(*)                       ! rank selector                     (20)
      print "(a)",'This should not be printed!'
      end select
      read(a,*,iostat=i) n(1),n(2)  ! list-directed internal read       (21)
      if (i.ne.0) return 1
      print '(a)', 'Enter anything to continue.'
      read(*,'(a)',iostat=i)ch      ! default input unit                (22)
      write(b,'(i1)',iostat=i)666   ! writes '*' as 666 won't fit in i1 (23) 
      end subroutine input
      end program test23stars
