const sortColorsE = () => {
  /**
 Do not return anything, modify nums in-place instead.
 */
  const sortColors = (nums: number[]): void => {
    let lastRed = -1;
    let lastWhite = -1;
    let lastBlue = -1;
    for (let i = 0; i < nums.length; i += 1) {
      if (nums[i] === 0) {
        lastRed += 1;
        if (nums[lastRed] === 1) {
          lastWhite += 1;
          if (nums[lastWhite] === 2) {
            lastBlue += 1;
          }
        } else if (nums[lastRed] === 2) {
          lastBlue += 1;
        }
      } else if (nums[i] === 1) {
        lastWhite = lastWhite === -1 ? lastRed + 1 : lastWhite + 1;
        if (nums[lastWhite] === 2) {
          lastBlue += 1;
        }
      } else if (nums[i] === 2) {
        lastBlue = i;
      }
      if (lastRed > -1) {
        // eslint-disable-next-line no-param-reassign
        nums[lastRed] = 0;
      }
      if (lastWhite > -1) {
        // eslint-disable-next-line no-param-reassign
        nums[lastWhite] = 1;
      }
      if (lastBlue > -1) {
        // eslint-disable-next-line no-param-reassign
        nums[lastBlue] = 2;
      }
    }
  };

  const ns1 = [2, 0, 2, 1, 1, 0];
  sortColors(ns1);
  console.log(ns1, [0, 0, 1, 1, 2, 2]);

  const ns2 = [2, 0, 1];
  sortColors(ns2);
  console.log(ns2, [0, 1, 2]);

  const ns3 = [1, 0, 1, 0, 0, 1, 1];
  sortColors(ns3);
  console.log(ns3, [0, 0, 0, 1, 1, 1, 1]);
};

sortColorsE();
