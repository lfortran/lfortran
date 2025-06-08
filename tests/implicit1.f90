program implicit1
! AST only
implicit none
IMPLICIT NONE
implicit none ()
implicit none (external)
implicit none (type)
implicit none (external, type)
implicit none (type, external)

implicit real (a-h,o-z)
implicit real (dp) (a-h,o-z)
implicit real*8 (a-h,o-z)
implicit real(8) (a-h,o-z)

implicit double precision (a-h,o-z)

implicit character (c,o-z)
implicit character (id) (a-z)

implicit integer (i-n)
implicit integer (i,j,k,l,m,n)
implicit integer (i,j-l,m,n)
implicit integer (dp) (a-h,o-z)
implicit integer*8 (i,j-l,m,n)
IMPLICIT INTEGER (A, C)
IMPLICIT INTEGER*4 (C, D-x)
IMPLICIT INTEGER(4) (C, D-x)

implicit logical (l, u-z)
implicit logical (dp) (a-h,o-z)
implicit logical*4 (l, u-z)
implicit logical(4) (l, u-z)

implicit complex (z)
implicit complex (dp) (a-h,o-z)
IMPLICIT COMPLEX (C)
implicit complex*8 (z)
implicit complex(4) (z)

IMPLICIT TYPE(BLOB) (A)
IMPLICIT class(X) (A-b)

implicit real(kind=4)(a), real(4+4)(b), real(d)(d), real(d(2))(a-z)
implicit integer(kind=4)(a), integer(4+4)(b), integer(d)(d), integer(d(2))(a-z)
implicit logical(kind=4)(a), logical(4+4)(b), logical(d)(d), logical(d(2))(a-z)
implicit complex(kind=4)(a), complex(4+4)(b), complex(d)(d), complex(d(2))(a-z)
implicit character(kind=c_char, len=4)(a)
implicit character(len=4, kind=c_char)(a)
implicit character(kind=c_char)(a)


end program implicit1
