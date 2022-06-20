const editDistance = () => {
  const buildMatrix = (word1: string, word2: string): number[][] => {
    const mx = new Array(word1.length + 1)
      .fill(0)
      .map(() => new Array(word2.length + 1).fill(0));
    for (let i = 0; i <= word1.length; i += 1) {
      mx[i][0] = i;
    }
    for (let i = 0; i <= word2.length; i += 1) {
      mx[0][i] = i;
    }
    for (let i = 1; i <= word1.length; i += 1) {
      for (let j = 1; j <= word2.length; j += 1) {
        const substitutionCost = word1[i - 1] === word2[j - 1] ? 0 : 1;
        mx[i][j] = Math.min(
          mx[i - 1][j] + 1,
          mx[i][j - 1] + 1,
          mx[i - 1][j - 1] + substitutionCost,
        );
      }
    }
    return mx;
  };

  const minDistance = (word1: string, word2: string): number =>
    buildMatrix(word1, word2)[word1.length][word2.length];

  console.log(minDistance('', ''), 0);
  console.log(minDistance('horse', 'ros'), 3);
  console.log(minDistance('intention', 'execution'), 5);
};

editDistance();
