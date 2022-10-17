#!/usr/bin/python3

import unittest

from compiled.math import avg, compose, mean, msort, neg, qsort, total, tail


class TestMath(unittest.TestCase):

    def test_avg(self):
        self.assertEqual(avg(6)(10), 8)

    def test_compose(self):
        self.assertEqual(compose(lambda x: x + 1)(lambda y: y * 2)(3), 7)

    def test_mean(self):
        self.assertEqual(mean([5, 10, 15]), 10)

    def test_quadruple(self):
        self.assertEqual(msort([1,4,2,5,7,8,3,6]), [1,2,3,4,5,6,7,8])

    def test_neg(self):
        self.assertEqual(neg(7), -7)

    def test_quadruple(self):
        self.assertEqual(qsort([1,4,2,5,7,8,3,6]), [1,2,3,4,5,6,7,8])

    def test_total(self):
        self.assertEqual(total([1,2,3,4,5]), 15)

    def test_tail(self):
        self.assertEqual(tail([1,2,3]), [2,3])

if __name__ == '__main__':
    unittest.main()
