const maximumPointsYouCanObtainFromCards = () => {
  const maxScore = (cardPoints: number[], k: number): number => {
    let sum = 0;
    for (let i = 0; i < k; i += 1) {
      sum += cardPoints[i];
    }
    let max = sum;
    for (let i = k - 1; i >= 0; i -= 1) {
      sum -= cardPoints[i];
      sum += cardPoints[cardPoints.length - k + i];
      if (sum > max) {
        max = sum;
      }
    }
    return max;
  };

  console.log(maxScore([1, 2, 3, 4, 5, 6, 1], 3), 12);

  console.log(maxScore([2, 2, 2], 2), 4);

  console.log(maxScore([9, 7, 7, 9, 7, 7, 9], 7), 55);
};

maximumPointsYouCanObtainFromCards();
