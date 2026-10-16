program continue_compilation_unterminated_string
print *, '[abc
print *, "abc&   ! trailing
   &def"
end program
