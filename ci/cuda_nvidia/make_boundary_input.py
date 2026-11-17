"""Write a FADV2D1 raw input with nonzero, non-smooth boundaries on a nonsquare grid."""
import sys
import numpy as np
out, nx, ny, steps, dt = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]), float(sys.argv[5])
rng = np.random.default_rng(20261019)
X, Y = np.meshgrid(np.linspace(0, 1, nx + 2), np.linspace(0, 1, ny + 2), indexing="ij")
s = (0.5 + 0.3 * np.sin(3.1 * X) * np.cos(2.3 * Y) + 0.05 * rng.standard_normal(X.shape)).astype(np.float32)
s[0, :] = 1.0 + 0.1 * np.arange(ny + 2)          # distinct nonzero values on each boundary
s[-1, :] = -0.75 + 0.05 * np.arange(ny + 2)
s[:, 0] = 2.0 - 0.07 * np.arange(nx + 2)
s[:, -1] = 0.3 + 0.11 * np.arange(nx + 2)
u = (0.4 + 0.2 * rng.standard_normal((nx + 1, ny + 2))).astype(np.float32)
v = (-0.3 + 0.2 * rng.standard_normal((nx + 2, ny + 1))).astype(np.float32)
dt32 = np.float32(dt)
with open(out, "wb") as f:
    f.write(b"FADV2D1\n")
    f.write(f"{nx} {ny} {steps} {float(dt32):.16E}\n".encode())
    f.write(b"mode raw\n")
    for a in (s, u, v):
        f.write(np.asfortranarray(a).astype("<f4").tobytes(order="F"))
print(out, s.shape, u.shape, v.shape)
