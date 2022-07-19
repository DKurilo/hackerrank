const pascalsTriangle = () => {
  const generate = (numRows: number): number[][] => {
    const triangle: number[][] = [[1]];
    if (numRows === 1) {
      return triangle;
    }
    triangle.push([1, 1]);
    for (let i = 2; i < numRows; i += 1) {
      triangle.push([]);
      const row = triangle[i];
      row.push(1);
      for (let j = 0; j < i - 1; j += 1) {
        row.push(triangle[i - 1][j] + triangle[i - 1][j + 1]);
      }
      row.push(1);
    }
    return triangle;
  };

  console.log(generate(5), [
    [1],
    [1, 1],
    [1, 2, 1],
    [1, 3, 3, 1],
    [1, 4, 6, 4, 1],
  ]);

  console.log(generate(1), [[1]]);

  console.log(generate(30));
};

pascalsTriangle();
