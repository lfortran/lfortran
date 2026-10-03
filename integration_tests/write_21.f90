program write_21
  implicit none
  character :: c(6)*3
  character(80) :: line
  data c/'ab ','cd ','ef ','gh ','ij ','kl '/
  write(line, "(*(A))") 'c = "', c, '"'
  if (trim(line) /= 'c = "ab cd ef gh ij kl "') error stop
  write(*, "(*(A))") 'c = "', c, '"'

  c = ['ab','cd','ef','gh','ij','kl']
  write(line, "(*(A))") 'c = "', c, '"'
  if (trim(line) /= 'c = "ab cd ef gh ij kl "') error stop
  write(*, "(*(A))") 'c = "', c, '"'

  c = ['ab ','cd ','ef ','gh ','ij ','kl ']
  write(line, "(*(A))") 'c = "', c, '"'
  if (trim(line) /= 'c = "ab cd ef gh ij kl "') error stop
  write(*, "(*(A))") 'c = "', c, '"'
end program write_21
