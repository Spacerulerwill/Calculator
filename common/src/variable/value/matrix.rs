use num::Zero;
use num_complex::Complex64;
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::variable::value::complex_to_string;

/// Matrix for use in calculator operations.
#[derive(Debug, Clone)]
pub struct Matrix {
    pub rows: Vec<Vec<Complex64>>,
}

impl Matrix {
    /// Panics if zero sized, or uneven row lengths
    pub fn from_rows(rows: Vec<Vec<Complex64>>) -> Self {
        // Can't be empty
        assert!(!rows.is_empty());
        // Can't have uneven row lengths
        let first_len = rows[0].len();
        assert!(rows.iter().all(|v| v.len() == first_len));
        Self { rows }
    }

    pub fn identity(size: usize) -> Self {
        let mut rows = vec![vec![Complex64::zero(); size]; size];
        for i in 0..size {
            rows[i][i] = Complex64::from(1.0);
        }
        Self::from_rows(rows)
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

        Self::from_rows(rows)
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

    pub fn determinant(&self) -> Complex64 {
        assert!(self.rows() == self.cols());

        let size = self.rows();

        match size {
            1 => self.rows[0][0],
            2 => self.rows[0][0] * self.rows[1][1] - self.rows[0][1] * self.rows[1][0],
            _ => {
                let mut det = Complex64::new(0.0, 0.0);

                for col in 0..size {
                    let sub_matrix = self.get_submatrix(0, col);
                    let cofactor = self.rows[0][col]
                        * sub_matrix.determinant()
                        * if col % 2 == 0 {
                            Complex64::new(1.0, 0.0)
                        } else {
                            Complex64::new(-1.0, 0.0)
                        };
                    det += cofactor;
                }

                det
            }
        }
    }

    /// Get a matrix by removing a row or column
    pub fn get_submatrix(&self, row_to_remove: usize, col_to_remove: usize) -> Matrix {
        let mut sub_rows = Vec::new();

        for (i, row) in self.rows.iter().enumerate() {
            if i == row_to_remove {
                continue;
            }
            let mut new_row = Vec::new();
            for (j, val) in row.iter().enumerate() {
                if j == col_to_remove {
                    continue;
                }
                new_row.push(*val);
            }
            sub_rows.push(new_row);
        }

        Matrix::from_rows(sub_rows)
    }

    pub fn inverse(&self) -> Option<Matrix> {
        // Step 1: Ensure the matrix is square
        if self.rows() != self.cols() {
            panic!("Non-square matrix passed to inverse function");
        }

        // Step 2: Calculate the determinant
        let determinant = self.determinant();
        if determinant == Complex64::zero() {
            // The matrix is singular, no inverse exists
            return None;
        }

        // Step 3: Compute the cofactor matrix
        let size = self.rows();
        let mut cofactor_matrix = vec![vec![Complex64::zero(); size]; size];

        for row in 0..size {
            for col in 0..size {
                // Get the submatrix without the current row and column
                let sub_matrix = self.get_submatrix(row, col);

                // Calculate the cofactor with alternating signs
                let cofactor = sub_matrix.determinant()
                    * if (row + col) % 2 == 0 {
                        Complex64::new(1.0, 0.0)
                    } else {
                        Complex64::new(-1.0, 0.0)
                    };

                cofactor_matrix[row][col] = cofactor;
            }
        }

        // Step 4: Transpose the cofactor matrix to get the adjugate matrix
        let adjugate_matrix = Matrix::from_rows(cofactor_matrix).transpose();

        // Step 5: Divide the adjugate matrix by the determinant
        Some(&adjugate_matrix / determinant)
    }
}

impl<'a> Add<&'a Matrix> for &'a Matrix {
    type Output = Matrix;

    fn add(self, rhs: &'a Matrix) -> Self::Output {
        assert!(self.dimensions() == rhs.dimensions());
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
        assert!(self.dimensions() == rhs.dimensions());
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
        assert!(self.cols() == rhs.rows());

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
