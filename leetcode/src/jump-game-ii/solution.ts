type Either<A, B> = { tag: 'Left'; left: A } | { tag: 'Right'; right: B };

const left = <A>(x: A): Either<A, never> => ({ tag: 'Left', left: x });
const right = <B>(x: B): Either<never, B> => ({ tag: 'Right', right: x });
const isLeft = (
  x: Either<unknown, unknown>,
): x is { tag: 'Left'; left: unknown } => x.tag === 'Left';
const isRight = (
  x: Either<unknown, unknown>,
): x is { tag: 'Right'; right: unknown } => x.tag === 'Right';

const loop = <A, B>(act: (x: A) => Either<A, B>) => (x: A): B => {
  let x1 = act(x);
  // eslint-disable-next-line no-constant-condition
  while (true) {
    if (isRight(x1)) {
      return x1.right;
    }
    x1 = act(x1.left);
  }
};

const mkArray = (from: number, to: number): number[] =>
  to - from + 1 <= 0
    ? []
    : [...new Array(to - from + 1)].map((_, i) => i + from);

const jump = (nums: number[]): number => {
  const doer = ([i, jumps]: [number, number]): Either<
    [number, number],
    number
  > =>
    i + 1 >= nums.length
      ? right(jumps)
      : mkArray(i + 1, i + nums[i]).reduce(
          (
            ej: Either<[number, number], number>,
            j: number,
          ): Either<[number, number], number> => {
            if (isRight(ej)) {
              return ej;
            }
            const [k, jumps1] = ej.left;
            if (j >= nums.length - 1) {
              return right(jumps1);
            }
            if (j + nums[j] >= k + nums[k]) {
              return left([j, jumps1]);
            }
            return ej;
          },
          left([i + 1, jumps + 1]),
        );
  return loop(doer)([0, 0]);
};

console.log(jump([2, 3, 1, 1, 4]), 2);
console.log(jump([2, 3, 0, 1, 4]), 2);
console.log(jump(mkArray(0, 1000).map(() => Math.trunc(Math.random() * 3))));
for (let i = 0; i < 10000; i += 1) {
  jump(mkArray(0, 1000).map(() => Math.trunc(Math.random() * 3)));
}
