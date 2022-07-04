const candyExec = () => {
  const candySlow = (ratings: number[]): number => {
    const candies: number[] = [];
    for (let i = 0; i < ratings.length; i += 1) {
      if (i === 0) {
        candies.push(1);
      } else if (ratings[i] <= ratings[i - 1]) {
        candies.push(1);
        if (candies[i - 1] === 1) {
          let k = i;
          while (ratings[k - 1] > ratings[k] && candies[k - 1] <= candies[k]) {
            candies[k - 1] += 1;
            k -= 1;
          }
        }
      } else {
        candies.push(candies[candies.length - 1] + 1);
      }
    }
    let sum = 0;
    for (let i = 0; i < candies.length; i += 1) {
      sum += candies[i];
    }
    return sum;
  };

  const candy = (ratings: number[]): number => {
    let candies = 1;
    let topFromLeftSlope = 1;
    let currentSlopeLen = 0;
    let currentDirection = 0;
    for (let i = 0; i < ratings.length; i += 1) {
      currentSlopeLen += 1;
      if (
        currentDirection === 1 &&
        (i === ratings.length - 1 || ratings[i + 1] <= ratings[i])
      ) {
        // Up slope
        // change direction /\ or /-
        candies += (currentSlopeLen * (currentSlopeLen + 1)) / 2 - 1;
        topFromLeftSlope = currentSlopeLen;
        currentSlopeLen = 1;
        currentDirection = Math.sign(ratings[i + 1] - ratings[i]);
      } else if (
        currentDirection === -1 &&
        (i === ratings.length - 1 || ratings[i + 1] >= ratings[i])
      ) {
        // Down slope
        // change direction \/ or \_
        candies +=
          (currentSlopeLen * (currentSlopeLen + 1)) / 2 -
          Math.min(topFromLeftSlope, currentSlopeLen);
        currentSlopeLen = 1;
        currentDirection = Math.sign(ratings[i + 1] - ratings[i]);
      } else if (
        currentDirection === 0 &&
        (i === ratings.length - 1 || ratings[i + 1] !== ratings[i])
      ) {
        // Plain to slope _/ or -\
        candies += currentSlopeLen - 1;
        currentSlopeLen = 1;
        topFromLeftSlope = 1;
        currentDirection = Math.sign(ratings[i + 1] - ratings[i]);
      }
    }
    return candies;
  };

  console.log(candy([1, 0, 2]), 5);

  console.log(candy([1, 2, 2]), 4);

  console.log(
    candy(
      new Array(20000).fill(0).map(() => Math.floor(Math.random() * 20000)),
    ),
  );

  console.log(candy([1, 3, 2, 2, 1]), 7);

  console.log(candy([1, 2, 3, 1, 0]), 9);
};

candyExec();
