use num_complex::Complex64;
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::expr::complex_to_string;

/// Matrix for use in calculator operations.
#[derive(Debug, Clone)]
pub struct Matrix {
    pub rows: Vec<Vec<Complex64>>,
}

impl Matrix {
    /// Panics if zero sized, or uneven row lengths
    pub fn from_rows(rows: Vec<Vec<Complex64>>) -> Self {
        // Can't be empty
        if rows.is_empty() {
            panic!("Attempt to create zero size matrix");
        }
        // Can't have uneven row lengths
        let first_len = rows[0].len();
        if rows.iter().any(|v| v.len() != first_len) {
            panic!("Attempt to create vector with uneven rows")
        }
        Matrix { rows }
    }

    pub fn rows(&self) -> usize {
        self.rows.len()
    }

    pub fn cols(&self) -> usize {
        self.rows[0].len()
    }

    pub fn dimensions(&self) -> (usize, usize) {
        (self.rows(), self.cols())
    }

    pub fn get(&self, row: usize, col: usize) -> Complex64 {
        return self.rows[row][col];
    }
}

impl<'a> Add<&'a Matrix> for &'a Matrix {
    type Output = Matrix;

    fn add(self, rhs: &'a Matrix) -> Self::Output {
        if self.dimensions() != rhs.dimensions() {
            panic!("Matrix dimensions do not match for addition");
        }

        let rows = self
            .rows
            .iter()
            .zip(rhs.rows.iter())
            .map(|(row1, row2)| {
                row1.iter()
                    .zip(row2.iter())
                    .map(|(val1, val2)| *val1 + *val2) // Add corresponding elements
                    .collect()
            })
            .collect();

        Matrix { rows }
    }
}

impl<'a> Sub<&'a Matrix> for &'a Matrix {
    type Output = Matrix;

    fn sub(self, rhs: &'a Matrix) -> Self::Output {
        if self.dimensions() != rhs.dimensions() {
            panic!("Matrix dimensions do not match for addition");
        }

        self + (&-rhs)
    }
}

impl<'a> Mul<Complex64> for &'a Matrix {
    type Output = Matrix;

    fn mul(self, scalar: Complex64) -> Self::Output {
        let rows = self
            .rows
            .iter()
            .map(|row| row.into_iter().map(|val| val * scalar).collect())
            .collect();

        Matrix { rows }
    }
}

impl<'a> Mul<&'a Matrix> for Complex64 {
    type Output = Matrix;

    fn mul(self, matrix: &'a Matrix) -> Self::Output {
        matrix * self
    }
}

impl<'a> Div<Complex64> for &'a Matrix {
    type Output = Matrix;

    fn div(self, scalar: Complex64) -> Self::Output {
        self * (1.0 / scalar)
    }
}

impl<'a> Neg for &'a Matrix {
    type Output = Matrix;

    fn neg(self) -> Matrix {
        return self * Complex64::from(-1.0);
    }
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut string = String::from("[");
        for (idx, row) in self.rows.iter().enumerate() {
            let mut row_str = if idx > 0 {
                String::from(" ")
            } else {
                String::new()
            };
            row_str += &row
                .iter()
                .map(|e| complex_to_string(e))
                .collect::<Vec<_>>()
                .join(", ");
            if idx != self.rows() - 1 {
                row_str += "\n";
            }
            string.push_str(&row_str);
        }
        string.push(']');
        write!(f, "{string}")
    }
}
