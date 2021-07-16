const mySqrt = (x: number): number => {
  const doer = (minY: number, maxY: number): number => {
    if (minY === maxY) {
      return minY;
    }
    const middleY = Math.trunc((minY + maxY) / 2);
    const y2 = middleY * middleY;
    if (y2 === x || middleY === minY || middleY === maxY) {
      return middleY;
    }
    if (y2 > x) {
      return doer(minY, middleY);
    }
    return doer(middleY, maxY);
  };
  return x === 1 || x === 0 ? x : doer(0, x);
};

console.log(mySqrt(0), 0);
console.log(mySqrt(1), 1);
console.log(mySqrt(4), 2);
console.log(mySqrt(8), 2);
console.log(mySqrt(2147483647), 46340);
