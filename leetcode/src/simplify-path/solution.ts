const processPathPart = (path: string[], part: string): string[] => {
  switch (part) {
    case '..':
      if (path.length === 0) {
        return path;
      }
      return path.slice(0, path.length - 1);
    case '':
      return path;
    case '.':
      return path;
    default:
      return path.concat(part);
  }
};

const simplifyPath = (path: string): string =>
  `/${path.split('/').reduce(processPathPart, []).join('/')}`;

console.log(simplifyPath('/home/'), '/home');
console.log(simplifyPath('/../'), '/');
console.log(simplifyPath('/home//foo/'), '/home/foo');
console.log(simplifyPath('/a/./b/../../c/'), '/c');
