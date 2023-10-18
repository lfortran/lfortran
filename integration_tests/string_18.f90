program string_18
print *, "123 " < "123"
if ("123 " < "123") error stop
print *, "123 " == "123"
if (.not. ("123 " == "123")) error stop
print *, "123 " > "123"
if ("123 " > "123") error stop
end
