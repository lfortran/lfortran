program expr_05
! Test parantheses in expressions
implicit none
integer :: x, a, b, c
logical :: l
a = 3
b = 4
c = 5

x = 2*3
x = -2*3
x = 2*(-3)
x = -2*(-3)
x = (-2)*(-3)
x = -2**3
x = -(2**3)
x = -(2*3)
x = 2**(-3)

x = a
x = (a)
x = a*b
x = -a*b
x = -(a*b)
x = a*(-b)
x = (-a)*(-b)

x = a*b*c
x = -a*b*c
x = a*(-b)*c
x = a*b*(-c)
x = (-a)*(-b)*(-c)

x = 3+4*5
x = (3+4)*5
x = a*(b + 5*(c - b))
x = (3-2*a*b)*5
x = (-2*a*b+3)*5
x = (-2*a*b+3*b*a)*5
x = (-2*(a/b)+(a+(-b))**2)*5
x = (+2*a*b+3)*5
x = a**2 + 2*a*b + b**2
x = (a + b)*(a - b)
x = (a + b)**2
x = (a + b)*(a**2 - a*b + b**2)
x = (a - b)*(a + b)*(a**2 + b**2)

x = 1/(a*b)
x = 1/a*b
x = (1/a)*b

x = 1/(a*b+1)
x = 1/a*b+1
x = (1/a)*b+1

x = 2-(-2)
x = a-(-b-c)
x = a-(-2*b)
x = c-(-2/b)
x = a-(2+3+4)
x = a+(2+3+4)
x = 2*a+a*b-(a*b+2*a)
x = 2*a+a*b-(a*b-2*a)
x = a-(b-(c-1))
x = a-b
x=  a-(b-c)
x=  a-b-c

x = -(a-(-b+(-b-(-b*b))))
x = -(3+5)
x = -(a+5)

l = x**3*4+a <= 4 .or. x<5 .and. x<6 .eqv. .true. .or. .not. .false. .and..true.
l = l .or. l .and. l
l = (l .or. l) .and. l
l = l .and. l .or. l
l = l .and. (l .or. l)
l = l .or. .not. l .and. l
l = l .or. l .and. .not. l
l = l .and. l .or. .not. l
l = l .and. .not. l .or. l
l = l .and. .not. (l .or. l)
end program
