double precision function diff(x, y)
double precision, intent(in) :: x, y
diff = x - y
end function

double precision function expand(x)
double precision, intent(in) :: x
expand = 2*x + 1
end function

double precision function has(x, y)
double precision, intent(in) :: x, y
has = x + y
end function

double precision function pi()
pi = 3
end function

double precision function stringconcat(x, y)
double precision, intent(in) :: x, y
stringconcat = x * y
end function

double precision function exp2(x)
double precision, intent(in) :: x
exp2 = 2*x + 1
end function

double precision function expm1(x)
double precision, intent(in) :: x
expm1 = x - 1
end function

double precision function fix(x)
double precision, intent(in) :: x
fix = x + 1
end function

double precision function trunc(x)
double precision, intent(in) :: x
trunc = 3*x
end function

double precision function floordiv(x, y)
double precision, intent(in) :: x, y
floordiv = x / y
end function

double precision function fma(x, y, z)
double precision, intent(in) :: x, y, z
fma = x*y - z
end function
