#!/usr/bin/env python3
"""Exercise the build-time DWARF converter's text and binary outputs."""

from pathlib import Path
import struct
import subprocess
import sys
import tempfile
import unittest


CONVERTER = Path(__file__).resolve().parents[2] / "libasr" / "dwarf_convert.py"


def line_table(directory, filename, rows):
    return f'''debug_line[0x00000000]
Line table prologue:
include_directories[  1] = "{directory}"
file_names[  1]:
           name: "{filename}"
      dir_index: 1

Address            Line   Column File   ISA Discriminator Flags
------------------ ------ ------ ------ --- ------------- -------------
{rows}

'''


class TestDwarfConvert(unittest.TestCase):
    def convert(self, directory, tables):
        raw = directory / "raw.txt"
        text = directory / "lines.txt"
        binary = directory / "lines.dat"
        raw.write_text(".debug_line contents:\n" + "".join(tables))
        subprocess.run([sys.executable, str(CONVERTER), str(raw), str(text),
                        str(binary)], check=True)
        metadata = text.read_text().splitlines()
        filename_count = int(metadata[0])
        filenames = metadata[1:filename_count + 1]
        row_count = int(metadata[filename_count + 1])
        data = binary.read_bytes()
        self.assertEqual(len(metadata), filename_count + 2)
        self.assertEqual(len(data), row_count * struct.calcsize("3Q"))
        return filenames, list(struct.iter_unpack("3Q", data))

    def test_unordered_tables_and_sequences(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary).resolve()
            # Both a later compilation unit and a later sequence start below
            # earlier rows. Equal addresses must retain their original order.
            first = line_table(directory / "first", "source.cpp", '''0x120 30 1 1 0 0 is_stmt
0x130 31 1 1 0 0 end_sequence
0x110 50 1 1 0 0 is_stmt
0x110 20 1 1 0 0 is_stmt
0x115 21 1 1 0 0 end_sequence''')
            second = line_table(directory / "second", "source.cpp", '''0x100 2 1 1 0 0 is_stmt
0x105 0 1 1 0 0
0x120 3 1 1 0 0 is_stmt
0x140 4 1 1 0 0 end_sequence''')
            filenames, rows = self.convert(directory, [first, second])
            self.assertEqual(filenames, [
                str(directory / "first" / "source.cpp"),
                str(directory / "second" / "source.cpp"),
            ])
            self.assertEqual(rows, [
                (0x100, 2, 1),
                (0x110, 50, 0),
                (0x110, 20, 0),
                (0x115, 21, 0),
                (0x120, 30, 0),
                (0x120, 3, 1),
                (0x130, 31, 0),
                (0x140, 4, 1),
            ])

    def test_ordered_rows_preserve_zero_line_and_end_sequence_handling(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary).resolve()
            table = line_table(directory, "source.cpp", '''0x100 2 1 1 0 0 is_stmt
0x110 0 1 1 0 0
0x120 3 1 1 0 0 end_sequence
0x130 0 1 1 0 0 end_sequence''')
            filenames, rows = self.convert(directory, [table])
            self.assertEqual(filenames, [str(directory / "source.cpp")])
            self.assertEqual(rows, [(0x100, 2, 0), (0x120, 3, 0)])


if __name__ == "__main__":
    unittest.main()
