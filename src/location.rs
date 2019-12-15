use std::fmt;
use std::ops::Add;

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Range {
    start: Point,
    end: Point,
}

impl Range {
    pub fn new(start: Point, end: Point) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> Point {
        self.start
    }

    pub fn end(&self) -> Point {
        self.end
    }
}

impl Add for Range {
    type Output = Range;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.line() == self.end.line() {
            write!(
                f,
                "[{}:{}-{}]",
                self.start.line(),
                self.start.column(),
                self.end.column()
            )
        } else {
            write!(
                f,
                "[{}-{}:{}-{}]",
                self.start.line(),
                self.end.line(),
                self.start.column(),
                self.end.column()
            )
        }
    }
}

/// Line, Column
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Point(usize, usize);

impl Point {
    pub fn new(line: usize, column: usize) -> Self {
        Self(line, column)
    }

    pub fn line(&self) -> usize {
        self.0
    }

    pub fn column(&self) -> usize {
        self.1
    }
}
