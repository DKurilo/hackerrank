const validAnagram = () => {
  const isAnagram = (s: string, t: string): boolean => {
    if (s.length !== t.length) {
      return false;
    }
    const map: Record<string, number> = {};
    for (let i = 0; i < s.length; i += 1) {
      map[s[i]] = map[s[i]] !== undefined ? map[s[i]] + 1 : 1;
      map[t[i]] = map[t[i]] !== undefined ? map[t[i]] - 1 : -1;
    }
    const chars = Object.keys(map);
    let res = true;
    for (let i = 0; i < chars.length; i += 1) {
      res = res && map[chars[i]] === 0;
    }
    return res;
  };

  console.log(isAnagram('anagram', 'nagaram'), true);

  console.log(isAnagram('car', 'rat'), false);
};

validAnagram();
