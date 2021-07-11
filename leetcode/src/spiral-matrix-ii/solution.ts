const absurd = <A>(_: never): A => {
  throw new Error('nope');
};

const generateMatrix = (n: number): number[][] => {
  const mx: number[][] = [...new Array(n)].map(() => new Array(n).fill(0));
  let i = 0;
  let j = 0;
  let iMin = 0;
  let iMax = n - 1;
  let jMin = 1;
  let jMax = n - 1;
  let direction: 'i+' | 'i-' | 'j+' | 'j-' = 'i+';
  for (let k = 0; k < n * n; k += 1) {
    mx[j][i] = k + 1;
    switch (direction) {
      case 'i+':
        if (i === iMax) {
          iMax -= 1;
          direction = 'j+';
          j += 1;
        } else {
          i += 1;
        }
        break;
      case 'i-':
        if (i === iMin) {
          iMin += 1;
          direction = 'j-';
          j -= 1;
        } else {
          i -= 1;
        }
        break;
      case 'j+':
        if (j === jMax) {
          jMax -= 1;
          direction = 'i-';
          i -= 1;
        } else {
          j += 1;
        }
        break;
      case 'j-':
        if (j === jMin) {
          jMin += 1;
          direction = 'i+';
          i += 1;
        } else {
          j -= 1;
        }
        break;
      default:
        absurd(direction);
        break;
    }
  }
  return mx;
};

console.log(generateMatrix(0));
console.log(generateMatrix(1));
console.log(generateMatrix(2));
console.log(generateMatrix(3));
console.log(generateMatrix(4));
console.log(generateMatrix(5));
