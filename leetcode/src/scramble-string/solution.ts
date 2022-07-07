const scrambleString = () => {
  const isScramble = (s1: string, s2: string): boolean => {
    if (s1.length !== s2.length) {
      return false;
    }
    const memo: Record<string, boolean> = {};
    const helper = (str1: string, str2: string): boolean => {
      const hash = `${str1}#${str2}`;
      if (memo[hash] !== undefined) {
        return memo[hash];
      }
      if (str1 === str2) {
        memo[hash] = true;
        return true;
      }
      if (str1.length === 1) {
        memo[hash] = false;
        return false;
      }
      for (let i = 1; i < str1.length; i += 1) {
        const str11 = str1.slice(0, i);
        const str12 = str1.slice(i);
        const str21 = str2.slice(0, i);
        const str22 = str2.slice(i);
        const str23 = str2.slice(0, str2.length - i);
        const str24 = str2.slice(str2.length - i);
        if (
          str12 + str11 === str2 ||
          (helper(str11, str21) && helper(str12, str22)) ||
          (helper(str11, str24) && helper(str12, str23))
        ) {
          memo[hash] = true;
          return true;
        }
      }
      memo[hash] = false;
      return false;
    };
    return helper(s1, s2);
  };

  console.log(isScramble('great', 'rgeat'), true);

  console.log(isScramble('abcde', 'caebd'), false);

  console.log(isScramble('a', 'a'), true);

  console.log(isScramble('abc', 'cba'), true);

  console.log(
    isScramble(
      'aaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaa',
      'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaba',
    ),
    true,
  );

  console.log(isScramble('ccabcbabcbabbbbcbb', 'bbbbabccccbbbabcba'), false);
};

scrambleString();
