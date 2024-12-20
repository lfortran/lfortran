integer,allocatable::y(:)
allocate(y(0:2))
y(:) = [1,2,3]
y(1:) = [10,20]
print "(3(1X,I0))",y
deallocate(y)
end program