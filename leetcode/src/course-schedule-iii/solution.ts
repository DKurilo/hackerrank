const courseScheduleIII = () => {
  type Duration = number;
  type LastDay = number;
  type Course = [Duration, LastDay];

  const duration = (c: Course): number => c[0];

  const lastDay = (c: Course): number => c[1];

  const isPossible = (c: Course): boolean => lastDay(c) >= duration(c);

  const scheduleCourse = (courses: Course[]): number => {
    const possibleCourses = courses.filter(isPossible);
    possibleCourses.sort((ca, cb) => lastDay(ca) - lastDay(cb));
    let days = 0;
    const placed = [];
    for (let i = 0; i < possibleCourses.length; i += 1) {
      days += duration(possibleCourses[i]);
      placed.push(duration(possibleCourses[i]));

      if (days > lastDay(possibleCourses[i])) {
        let max = 0;
        for (let k = 0; k < placed.length; k += 1) {
          if (placed[k] > max) {
            max = placed[k];
          }
        }
        const toRemove: number = placed.indexOf(max);
        for (let k = toRemove + 1; k < placed.length; k += 1) {
          if (k > toRemove) {
            placed[k - 1] = placed[k];
          }
        }
        days -= max;
        placed.pop();
      }
    }
    return placed.length;
  };

  console.log(
    scheduleCourse([
      [2, 3],
      [6, 10],
      [7, 18],
      [4, 19],
      [5, 19],
      [3, 20],
      [10, 30],
    ]),
    6,
  );

  console.log(
    scheduleCourse([
      [100, 200],
      [200, 1300],
      [1000, 1250],
      [2000, 3200],
    ]),
    3,
  );

  console.log(scheduleCourse([[1, 2]]), 1);

  console.log(
    scheduleCourse([
      [3, 2],
      [4, 3],
    ]),
    0,
  );

  const rand = () => Math.floor(Math.random() * 10000) + 1;

  const cs1 = new Array(10000)
    .fill(0)
    .map((): [number, number] => [rand(), rand()]);
  console.log(scheduleCourse(cs1));

  const cs2 = new Array(1000)
    .fill(0)
    .map((): [number, number] => [Math.floor(rand() / 100) + 1, rand()])
    .map(
      (c: Course): Course => (isPossible(c) ? c : [lastDay(c), duration(c)]),
    );
  console.log(scheduleCourse(cs2));
};

courseScheduleIII();
