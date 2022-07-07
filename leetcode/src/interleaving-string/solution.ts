const interleavingString = () => {
  const isInterleave = (s1: string, s2: string, s3: string): boolean => {
    // just sanity check
    if (s1.length + s2.length !== s3.length) {
      return false;
    }

    // What we are going to have is a matrix like this:
    // s1 = 'ab', s2 = 'abca', s3 = 'ababca'
    //
    //  | |a|b|c|a
    // ------------
    //  |1| | | |
    // -----------
    // a| | | | |
    // -----------
    // b| | | | |
    //
    // Now we are going to fill in this matrix.
    // Each movement to the right means we took letter from second word.
    // Each movement to the left means we took letter from first word.
    // It's always possible to start, that's why we have 1 in top left corner.
    // And if we have 1 in some cell it means using letters on the same level and above from first word
    // and letters in on the same level and on the left side from second word we can build part
    // of the third word.
    // Let's fill this matrix:
    //  | |a|b|c|a
    // ------------
    //  |1|1|1|0|0
    // -----------
    // a|1|0|1|0|0
    // -----------
    // b|1|1|1|1|1
    //
    // so we have to ways to get final word:
    // 1. 1 1 2 2 1 1
    // 2. 2 2 1 1 1 1
    //
    // Most interesting thing here is to fill next cell we just need to know if it's possible to build
    // part of the word on cell above or on cell on the left side. And surely we need to check if the next
    // letter is the same as in final word.
    const matrix: boolean[][] = new Array(s1.length + 1)
      .fill(0)
      .map(() => new Array(s2.length + 1).fill(false));
    matrix[0][0] = true;
    for (let i = 0; i < s1.length; i += 1) {
      matrix[i + 1][0] = matrix[i][0] && s1[i] === s3[i];
    }
    for (let i = 0; i < s2.length; i += 1) {
      matrix[0][i + 1] = matrix[0][i] && s2[i] === s3[i];
    }

    for (let i = 1; i <= s1.length; i += 1) {
      for (let k = 1; k <= s2.length; k += 1) {
        matrix[i][k] =
          (matrix[i - 1][k] && s1[i - 1] === s3[i + k - 1]) ||
          (matrix[i][k - 1] && s2[k - 1] === s3[i + k - 1]);
      }
    }

    // finally in cell in the right bottom corner we have answer.
    // It answers on question if it possible to build the third word from
    // first and second words adding letters in the same order as they appeared in words.
    return matrix[s1.length][s2.length];
  };

  console.log(isInterleave('ab', 'abca', 'ababca'), true);

  console.log(isInterleave('aabcc', 'dbbca', 'aadbbcbcac'), true);

  console.log(isInterleave('aabcc', 'dbbca', 'aadbbbaccc'), false);

  console.log(isInterleave('', '', ''), true);

  const s1: string = new Array(100).fill('a').join('');
  const s2: string = new Array(100).fill('a').join('');
  const s3: string = new Array(200).fill('a').join('');
  console.log(isInterleave(s1, s2, s3), true);

  console.log(isInterleave('a', 'b', 'a'), false);

  console.log(isInterleave('aabcc', 'dbbca', 'aadbbcbcacf'), false);

  console.log(isInterleave('aabcc', 'dbbca', 'aadbbcbcaf'), false);

  console.log(isInterleave('aabd', 'abdc', 'aabdabcd'), true);

  console.log(
    isInterleave(
      'aaaaaaaaaaaaaaaaaaaaaaaaaaa',
      'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    ),
    false,
  );

  console.log(
    isInterleave(
      'abababababababababababababababababababababababababababababababababababababababababababababababababbb',
      'babababababababababababababababababababababababababababababababababababababababababababababababaaaba',
      'abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababbb',
    ),
    false,
  );
};

interleavingString();
