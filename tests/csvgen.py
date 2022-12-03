#!/usr/bin/env python3

import argparse
import math


def generate(start, end, count, function):
  error = 1e-08
  step = (end - start) / count
  x = start
  while x - error < end:
    yield f'{x};{function(x)}'
    x += step


def main() -> int:
  parser = argparse.ArgumentParser(prog='csvgen',
    description='Generates csv table representing some function.',
    epilog='Originally written by come_ill_foo',
    add_help=True)

  parser.add_argument('-f', '--function', choices=['exp', 'exp2', 'log', 'log2', 'log10', 'sqrt', 'cos', 'sin', 'tan'], required=True)
  parser.add_argument('-s', '--start', type=float, required=True)
  parser.add_argument('-c', '--count', type=int, required=True)
  parser.add_argument('-e', '--end', type=float, required=True)

  args = parser.parse_args()

  for line in generate(args.start, args.end, args.count, getattr(math, args.function)):
    print(line)

  return 0


if __name__ == '__main__':
  main()
