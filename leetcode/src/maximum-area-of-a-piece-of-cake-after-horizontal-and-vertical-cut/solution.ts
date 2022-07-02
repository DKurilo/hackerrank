const maximumAreaOfAPieceOfCakeAfterHorizontalAndVerticalCut = () => {
  const maxArea = (
    h: number,
    w: number,
    horizontalCuts: number[],
    verticalCuts: number[],
  ): number => {
    const modulo = 10 ** 9 + 7;
    const sum = (x: number, y: number): number =>
      ((x % modulo) + (y % modulo)) % modulo;
    const mul = (x: number, y: number): number => {
      if (x < 10 ** 8 && y < 10 ** 8) {
        return (x * y) % modulo;
      }
      const x1 = Math.floor(x / 2);
      const x2 = Math.ceil(x / 2);
      const y1 = Math.floor(y / 2);
      const x1y1 = mul(x1, y1);
      const x1y2 = sum(x1y1, (y % 2) * x1);
      const x2y1 = sum(x1y1, (x % 2) * y1);
      const x2y2 = sum(x2y1, (y % 2) * x2);
      return sum(sum(sum(x1y1, x1y2), x2y1), x2y2);
    };

    const getMaxLength = (size: number, cuts: number[]) =>
      (({ prev, max }) => Math.max(size - prev, max))(
        cuts.reduce(
          ({ prev, max }, cut) => {
            return {
              prev: cut,
              max: Math.max(max, cut - prev),
            };
          },
          { prev: 0, max: 0 },
        ),
      );

    const asc = (a: number, b: number): number => a - b;

    horizontalCuts.sort(asc);
    verticalCuts.sort(asc);

    return mul(getMaxLength(h, horizontalCuts), getMaxLength(w, verticalCuts));
  };

  console.log(maxArea(5, 4, [1, 2, 4], [1, 3]), 4);

  console.log(maxArea(5, 4, [3, 1], [1]), 6);

  console.log(maxArea(5, 4, [3], [3]), 9);

  console.log(maxArea(987654, 998765, [1], [1]), 432253990);

  console.log(maxArea(9876543211, 9987654322, [1], [1]), 395218971);
};

maximumAreaOfAPieceOfCakeAfterHorizontalAndVerticalCut();
