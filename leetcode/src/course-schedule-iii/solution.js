var courseScheduleIII = function () {
    var nothing = function () { return ({ tag: 'Nothing' }); };
    var just = function (v) { return ({ tag: 'Just', value: v }); };
    var isJust = function (mb) { return mb.tag === 'Just'; };
    var isNothing = function (mb) { return mb.tag === 'Nothing'; };
    var fromMaybe = function (defval, mb) {
        return mb.tag === 'Just' ? mb.value : defval;
    };
    var course = function (lastDay, duration) { return ({
        lastDay: lastDay,
        duration: duration,
        required: false
    }); };
    var duration = function (c) { return c.duration; };
    var lastDay = function (c) { return c.lastDay; };
    var isRequired = function (c) { return c.required; };
    var isImposible = function (c) { return lastDay(c) < duration(c); };
    var setRequired = function (c) {
        // eslint-disable-next-line no-param-reassign
        c.required = true;
        return c;
    };
    var place = function (start, end, c) {
        if (start <= lastDay(c) - duration(c) && end >= lastDay(c)) {
            return just({ start: start, course: c });
        }
        return nothing();
    };
    var tryCourses = function (possibleCourses) {
        possibleCourses.sort(function (ca, cb) { return lastDay(ca) - lastDay(cb); });
        var latestDay = lastDay(possibleCourses[possibleCourses.length - 1]);
        return possibleCourses.reduce(function (_a, c) {
            var pls = _a[0], rs = _a[1];
            var startDay = pls.length > 0
                ? duration(pls[pls.length - 1].course) +
                    pls[pls.length - 1].start +
                    1
                : 1;
            var mbPl = place(startDay, latestDay, c);
            if (isJust(mbPl)) {
                return [pls.concat(mbPl.value), rs];
            }
            return [pls, rs.concat([c])];
        }, [[], []]);
    };
    var buildSchedule = function (courses, prev) {
        var _a = tryCourses(courses), placed = _a[0], rest = _a[1];
        var max = Math.max(placed.length, prev);
        if (placed.length >= prev && rest.length > 1) {
            var placedCourses = placed.map(function (pl) { return pl.course; });
            placedCourses.sort(function (ca, cb) { return duration(cb) - duration(ca); });
            var minD = Math.min.apply(Math, rest.map(duration));
            for (var i = 0; i < placedCourses.length; i += 1) {
                if (!isRequired(placedCourses[i]) &&
                    duration(placedCourses[i]) > minD) {
                    var newList = placedCourses
                        .slice(0, i)
                        .concat(placedCourses.slice(i + 1))
                        .concat(rest);
                    var newCourses = buildSchedule(newList, placedCourses.length);
                    if (newCourses < placedCourses.length) {
                        setRequired(placedCourses[i]);
                    }
                    max = Math.max(max, newCourses);
                }
            }
        }
        return max;
    };
    var scheduleCourse = function (courses) {
        var possibleCourses = courses
            .map(function (_a) {
            var dur = _a[0], lastD = _a[1];
            return course(lastD, dur);
        })
            .filter(function (c) { return !isImposible(c); });
        if (possibleCourses.length === 0) {
            return 0;
        }
        return buildSchedule(possibleCourses, 0);
    };
    // console.log(
    //   scheduleCourse([
    //     [2, 3],
    //     [6, 10],
    //     [7, 18],
    //     [4, 19],
    //     [5, 19],
    //     [3, 20],
    //     [10, 30],
    //   ]),
    //   5,
    // );
    // console.log(
    //   scheduleCourse([
    //     [100, 200],
    //     [200, 1300],
    //     [1000, 1250],
    //     [2000, 3200],
    //   ]),
    //   3,
    // );
    // console.log(scheduleCourse([[1, 2]]), 1);
    // console.log(
    //   scheduleCourse([
    //     [3, 2],
    //     [4, 3],
    //   ]),
    //   0,
    // );
    var rand = function () { return Math.floor(Math.random() * 10000) + 1; };
    var cs1 = new Array(100)
        .fill(0)
        .map(function () { return [rand(), rand()]; });
    console.log(scheduleCourse(cs1));
};
courseScheduleIII();
