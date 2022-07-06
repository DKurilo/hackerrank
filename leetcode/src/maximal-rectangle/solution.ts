const maximalRectangleEx = () => {
  // https://leetcode.com/problems/largest-rectangle-in-histogram/
  const largestRectangleArea = (heights: number[]): number => {
    const stack: number[] = [];
    let max = 0;
    stack.push(0);
    for (let i = 1; i < heights.length; i += 1) {
      if (heights[i] <= heights[stack[stack.length - 1]]) {
        while (
          stack.length > 0 &&
          heights[stack[stack.length - 1]] >= heights[i]
        ) {
          const h = heights[stack.pop()];
          max = Math.max(
            max,
            (stack.length === 0 ? i : i - stack[stack.length - 1] - 1) * h,
          );
        }
      }
      stack.push(i);
    }
    if (stack.length > 0) {
      const i = heights.length;
      while (stack.length > 0) {
        const h = heights[stack.pop()];
        max = Math.max(
          max,
          (stack.length === 0 ? i : i - stack[stack.length - 1] - 1) * h,
        );
      }
    }
    return max;
  };
  const maximalRectangle = (matrix: string[][]): number => {
    const histograms: number[][] = [];
    for (let i = 0; i < matrix.length; i += 1) {
      const histogram: number[] = [];
      for (let j = 0; j < matrix[i].length; j += 1) {
        let height = 0;
        for (let k = i; k >= 0 && matrix[k][j] === '1'; k -= 1) {
          height += 1;
        }
        histogram.push(height);
      }
      histograms.push(histogram);
    }
    return Math.max(...histograms.map(largestRectangleArea));
  };

  console.log(
    maximalRectangle([
      ['1', '0', '1', '0', '0'],
      ['1', '0', '1', '1', '1'],
      ['1', '1', '1', '1', '1'],
      ['1', '0', '0', '1', '0'],
    ]),
    6,
  );

  console.log(maximalRectangle([['0']]), 0);

  console.log(maximalRectangle([['1']]), 1);

  console.log(
    maximalRectangle(
      new Array(200)
        .fill(0)
        .map(() =>
          new Array(200)
            .fill(0)
            .map(() => Math.round(Math.random() + 0.4).toString()),
        ),
    ),
  );
};

maximalRectangleEx();
