subroutine add_or_sub(select, a, b)
integer :: select
real :: a, b
if (select == 1) goto 1
if (select == 2) goto 2

1 continue
return
2 continue
return
end subroutine

program goto_gototarget_return_01
real :: a, b
integer :: select
a = 1.0
b = 2.0
select = 1
call add_or_sub(select, a, b)
print *, a
select = 2
call add_or_sub(select, a, b)
print *, a
end program
