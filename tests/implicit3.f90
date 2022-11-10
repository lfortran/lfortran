integer function a(n,m,z,k)
implicit real (n,m)
implicit real*8 (k)
implicit complex (z)
n = 5
m = 8
k = 9.23
integer :: X(n), Y(m)
X = [1,2,3,4,5]
z = (1.0 , 2.0)
end function