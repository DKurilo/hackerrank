const grayCodeEx = () => {
  const grayCode = (n: number): number[] => {
    if (n === 1) {
      return [0, 1];
    }
    const highBit = 2 ** (n - 1);
    const grCode = grayCode(n - 1);
    const l = grCode.length;
    for (let i = l - 1; i >= 0; i -= 1) {
      grCode.push(grCode[i] + highBit);
    }
    return grCode;
  };

  console.log(grayCode(2), [0, 1, 3, 2]);

  console.log(grayCode(1), [0, 1]);

  console.log(
    grayCode(5).map((x) => x.toString(2)),
    [0, 1],
  );

  const grayCode16 = grayCode(16).map((x) => x.toString(2));
  console.log(
    grayCode16.slice(0, 10),
    grayCode16.slice(grayCode16.length - 10),
  );
};

grayCodeEx();
