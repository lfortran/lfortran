import numpy as np

# Desired 4D shape, e.g. (2, 2, 1, 1)
shape = (2, 2, 1, 1)

# Create complex-valued array
array = np.arange(np.prod(shape), dtype=np.complex64).reshape(shape)

# Convert to Fortran order and flatten for Fortran-style reading
flat_fortran = np.reshape(array, (-1,), order='F')

# Write to data.txt in Fortran format
with open("file_32_data.txt", "w") as f:
    for val in flat_fortran:
        f.write(f"({val.real},{val.imag}) ")
    f.write("\n")

