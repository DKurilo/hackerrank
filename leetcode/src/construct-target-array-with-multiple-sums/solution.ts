const constructTargetArrayWithMultipleSums = () => {
  const isPossible = (target: number[]): boolean => {
    // eslint-disable-next-line no-constant-condition
    while (true) {
      let maxIndex;
      let max = 0;
      let sum = 0;
      for (let i = 0; i < target.length; i += 1) {
        if (target[i] > max) {
          maxIndex = i;
          max = target[i];
        }
        sum += target[i];
      }
      if (max === 1) {
        return true;
      }
      sum -= max;
      if (max < sum + 1 || sum <= 0) {
        return false;
      }

      if (sum === 1) {
        return true;
      }

      // eslint-disable-next-line no-param-reassign
      target[maxIndex] %= sum;

      if (target[maxIndex] <= 0) {
        return false;
      }
    }
  };

  console.log(isPossible([9, 3, 5]), true);

  console.log(isPossible([1, 1, 1, 2]), false);

  console.log(isPossible([8, 5]), true);

  const arr1 = new Array(50000)
    .fill(0)
    .map(() => Math.floor(Math.random() * 1000000000) + 1);
  console.log(isPossible(arr1));

  console.log(isPossible([2, 900000001]), true);

  console.log(isPossible([2, 900000002]), false);

  console.log(isPossible([1, 5, 27]), true);

  console.log(isPossible([2]), false);
};

constructTargetArrayWithMultipleSums();
