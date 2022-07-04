const largestRectangleInHistogram = () => {
  const largestRectangleArea_my = (heights: number[]): number => {
    const stack: [number, number][] = [];
    let max = 0;
    for (let i = 0; i < heights.length; i += 1) {
      if (stack.length === 0 || heights[i] > stack[stack.length - 1][1]) {
        stack.push([i, heights[i]]);
      } else if (heights[i] < stack[stack.length - 1][1]) {
        let start = 0;
        while (stack.length > 0 && stack[stack.length - 1][1] >= heights[i]) {
          const pair = stack.pop();
          start = pair[0];
          const val = pair[1];
          max = Math.max(max, (i - start) * val);
        }
        stack.push([start, heights[i]]);
      }
    }
    while (stack.length > 0) {
      const [start, val] = stack.pop();
      max = Math.max(max, (heights.length - start) * val);
    }
    return max;
  };

  // canonical solution require stack with only index
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

  console.log(largestRectangleArea([2, 3]), 4);

  console.log(largestRectangleArea([2, 1, 5, 6, 2, 3]), 10);

  console.log(largestRectangleArea([2, 4]), 4);

  console.log(largestRectangleArea([2, 2, 2, 2]), 8);

  console.log(largestRectangleArea([]), 0); // canonical solution returns NaN here

  console.log(largestRectangleArea([0]), 0);

  console.log(largestRectangleArea([0, 2, 4, 3, 5, 2, 3, 1]), 12);
};

largestRectangleInHistogram();
