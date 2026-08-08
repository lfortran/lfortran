#!/usr/bin/env python3

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

import edn
import reduce


class EDNTests(unittest.TestCase):
    def test_round_trip(self):
        text = (
            '(ASRText :version 1 :value '
            '(TranslationUnit :symtab '
            '(SymbolTable :id 0 :symbols {"p" (Program :body [])}) '
            ':items []))'
        )
        self.assertEqual(edn.render(edn.parse(text)), text)

    def test_tag_and_escaped_string(self):
        text = '#asr/bytes "00\\nff"'
        self.assertEqual(edn.render(edn.parse(text)), text)

    def test_reduction_candidates(self):
        root = edn.parse(
            '(Node :optional (Child 4) :items [(A) (B)] :flag false)')
        candidates = reduce.reduction_candidates(root)
        descriptions = [candidate.describe() for candidate in candidates]
        self.assertTrue(any("replace" in item for item in descriptions))
        self.assertTrue(any("remove" in item for item in descriptions))

    def test_remove_vector_item(self):
        root = edn.parse('(Node :items [(A) (B)])')
        vector_path = next(
            path for path, node in edn.walk(root)
            if node.kind == "vector")
        candidate = reduce.apply_reduction(
            root, reduce.Reduction(vector_path, "remove", 0, 1))
        self.assertEqual(edn.render(candidate), '(Node :items [(B)])')


if __name__ == "__main__":
    unittest.main()
