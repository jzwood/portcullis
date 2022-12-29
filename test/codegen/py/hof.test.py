#!/usr/bin/python3

import unittest

from compiled.hof import double, id1, id2, one1, one2, quadruple

class TestHof(unittest.TestCase):

    def test_double(self):
        self.assertEqual(double(4), 8)

    def test_id(self):
        self.assertEqual(id1(2), 2)
        self.assertEqual(id2(3), 3)

    def test_one(self):
        self.assertEqual(one1, 1)
        self.assertEqual(one2, 1)

    def test_quadruple(self):
        self.assertEqual(quadruple(4.5), 18)

if __name__ == '__main__':
    unittest.main()
