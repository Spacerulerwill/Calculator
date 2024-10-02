use num::Zero;
use num_complex::Complex64;
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::value::complex_to_string;

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

    pub fn identity(size: usize) -> Matrix {
        let mut rows = vec![vec![Complex64::zero(); size]; size];
        for i in 0..size {
            rows[i][i] = Complex64::from(1.0);
        }
        Matrix::from_rows(rows)
    }

    pub fn transpose(&self) -> Matrix {
        let new_rows = self.cols();
        let new_cols = self.rows();

        let mut rows = vec![vec![Complex64::from(0.0); new_cols]; new_rows];

        for row in 0..new_rows {
            for col in 0..new_cols {
                rows[row][col] = self.rows[col][row];
            }
        }

        Matrix::from_rows(rows)
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

impl<'a> Mul<&'a Matrix> for &'a Matrix {
    type Output = Matrix;

    fn mul(self, rhs: &'a Matrix) -> Self::Output {
        if self.cols() != rhs.rows() {
            panic!("Matrix dimensions do not match for multiplication");
        }

        let mut result_rows = Vec::with_capacity(self.rows());

        for i in 0..self.rows() {
            let mut result_row = Vec::with_capacity(rhs.cols());
            for j in 0..rhs.cols() {
                let mut sum = Complex64::new(0.0, 0.0);
                for k in 0..self.cols() {
                    sum += self.get(i, k) * rhs.get(k, j);
                }
                result_row.push(sum);
            }
            result_rows.push(result_row);
        }
        Matrix::from_rows(result_rows)
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

// Implementing fmt::Display for Matrix
impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Convert each Complex64 to string before formatting
        let string_rows: Vec<Vec<String>> = self
            .rows
            .iter()
            .map(|row| row.iter().map(|c| complex_to_string(c)).collect())
            .collect();

        // Use your matrix_format to format the rows
        write!(f, "{}", matrix_format(&string_rows))
    }
}

pub fn matrix_format<T: ToString>(matrix: &Vec<Vec<T>>) -> String {
    if matrix.is_empty() {
        return String::from("[]");
    }

    let rows = matrix.len();
    let cols = matrix[0].len();

    // Convert the matrix elements to strings
    let string_matrix: Vec<Vec<String>> = matrix
        .iter()
        .map(|row| row.iter().map(|element| element.to_string()).collect())
        .collect();

    // Calculate the maximum width of each column
    let mut max_col_widths = Vec::with_capacity(cols);
    for col in 0..cols {
        let col_widths: Vec<usize> = string_matrix.iter().map(|row| row[col].len()).collect();
        let max = *col_widths.iter().max().unwrap_or(&0);
        max_col_widths.push(max);
    }

    let mut formatted_string = String::from("[");
    for (row_idx, row) in string_matrix.iter().enumerate() {
        // Create a vector to hold formatted elements for the row
        let formatted_row: Vec<String> = row
            .iter()
            .enumerate()
            .map(|(i, element)| {
                let width = max_col_widths[i];
                format!("{:^width$}", element, width = width)
            })
            .collect();

        // Row starts with an extra space if not the first one
        let mut formatted_row_string = if row_idx == 0 {
            String::new()
        } else {
            String::from(" ")
        };

        formatted_row_string.push_str(&formatted_row.join(", "));
        if row_idx != rows - 1 {
            formatted_row_string.push('\n');
        }
        formatted_string.push_str(&formatted_row_string);
    }
    formatted_string.push(']');
    formatted_string
}