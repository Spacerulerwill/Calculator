use num::Zero;
use num_complex::Complex64;
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::variable::value::complex_to_string;

/// Matrix for use in calculator operations - note that vectors are treated as single row or column matrices
/// and therefore matrix specific operations (cross/dot product) are implemented here
#[derive(Debug, Clone, PartialEq)]
pub struct Matrix {
    pub rows: Vec<Vec<Complex64>>,
}

impl Matrix {
    /// Panics if zero sized, or uneven row lengths
    pub fn from_rows(rows: Vec<Vec<Complex64>>) -> Self {
        // Can't have zero rows
        assert!(!rows.is_empty());
        // Can't have empty columns
        assert!(rows.iter().all(|v| !v.is_empty()));
        // Can't have uneven row lengths
        let first_len = rows[0].len();
        assert!(rows.iter().all(|v| v.len() == first_len));
        Self { rows }
    }

    pub fn identity(size: usize) -> Self {
        let mut rows = vec![vec![Complex64::zero(); size]; size];
        for (i, item) in rows.iter_mut().enumerate() {
            item[i] = Complex64::from(1.0);
        }
        Self::from_rows(rows)
    }

    pub fn transpose(&self) -> Matrix {
        let new_rows = self.cols();
        let new_cols = self.rows();

        let mut rows = vec![vec![Complex64::from(0.0); new_cols]; new_rows];

        for (row, row_item) in rows.iter_mut().enumerate() {
            for (col, col_item) in row_item.iter_mut().enumerate() {
                *col_item = self.rows[col][row];
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
        self.rows[row][col]
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
        assert!(
            self.rows() == self.cols(),
            "Cannot calculate inverse of non square matrix"
        );

        // Step 2: Calculate the determinant
        let determinant = self.determinant();
        if determinant == Complex64::zero() {
            // The matrix is singular, no inverse exists
            return None;
        }

        // Step 3: Compute the cofactor matrix
        let size = self.rows();
        if size == 1 {
            return Some(Matrix::from_rows(vec![vec![
                Complex64::from(1.0) / self.get(0, 0),
            ]]));
        }
        let mut cofactor_matrix = vec![vec![Complex64::zero(); size]; size];

        for (row, row_item) in cofactor_matrix.iter_mut().enumerate() {
            for (col, col_item) in row_item.iter_mut().enumerate() {
                // Get the submatrix without the current row and column
                let sub_matrix = self.get_submatrix(row, col);

                // Calculate the cofactor with alternating signs
                let cofactor = sub_matrix.determinant()
                    * if (row + col) % 2 == 0 {
                        Complex64::new(1.0, 0.0)
                    } else {
                        Complex64::new(-1.0, 0.0)
                    };

                *col_item = cofactor;
            }
        }

        // Step 4: Transpose the cofactor matrix to get the adjugate matrix
        let adjugate_matrix = Matrix::from_rows(cofactor_matrix).transpose();

        // Step 5: Divide the adjugate matrix by the determinant
        Some(&adjugate_matrix / determinant)
    }

    pub fn row_cross(&self, other: &Matrix) -> Matrix {
        assert_eq!(self.rows(), 1);
        assert_eq!(self.cols(), 3);
        assert_eq!(other.rows(), 1);
        assert_eq!(other.cols(), 3);

        let vec1 = &self.rows[0];
        let a1 = vec1[0];
        let a2 = vec1[1];
        let a3 = vec1[2];

        let vec2 = &other.rows[0];
        let b1 = vec2[0];
        let b2 = vec2[1];
        let b3 = vec2[2];

        Matrix::from_rows(vec![vec![
            a2 * b3 - a3 * b2,
            a3 * b1 - a1 * b3,
            a1 * b2 - a2 * b1,
        ]])
    }

    pub fn column_cross(&self, other: &Matrix) -> Matrix {
        assert_eq!(self.rows(), 3);
        assert_eq!(self.cols(), 1);
        assert_eq!(other.rows(), 3);
        assert_eq!(other.cols(), 1);

        let vec1: Vec<Complex64> = self.rows.iter().map(|x| x[0]).collect();
        let a1 = vec1[0];
        let a2 = vec1[1];
        let a3 = vec1[2];

        let vec2: Vec<Complex64> = other.rows.iter().map(|x| x[0]).collect();
        let b1 = vec2[0];
        let b2 = vec2[1];
        let b3 = vec2[2];

        Matrix::from_rows(vec![vec![
            a2 * b3 - a3 * b2,
            a3 * b1 - a1 * b3,
            a1 * b2 - a2 * b1,
        ]])
    }

    pub fn row_dot(&self, other: &Matrix) -> Complex64 {
        assert_eq!(self.rows(), 1);
        assert_eq!(other.rows(), 1);
        assert_eq!(self.cols(), other.cols());
        let left_values = &self.rows[0];
        let right_values = &other.rows[0];
        left_values
            .iter()
            .zip(right_values.iter())
            .map(|(x, y)| x * y)
            .sum()
    }

    pub fn column_dot(&self, other: &Matrix) -> Complex64 {
        assert_eq!(self.cols(), 1);
        assert_eq!(other.cols(), 1);
        assert_eq!(self.rows(), other.rows());
        let left_values: Vec<Complex64> = self.rows.iter().map(|x| x[0]).collect();
        let right_values: Vec<Complex64> = other.rows.iter().map(|x| x[0]).collect();
        left_values
            .iter()
            .zip(right_values.iter())
            .map(|(x, y)| x * y)
            .sum()
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

impl Mul<Complex64> for &Matrix {
    type Output = Matrix;

    fn mul(self, scalar: Complex64) -> Self::Output {
        let rows = self
            .rows
            .iter()
            .map(|row| row.iter().map(|val| val * scalar).collect())
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

impl Div<Complex64> for &Matrix {
    type Output = Matrix;

    fn div(self, scalar: Complex64) -> Self::Output {
        self * (Complex64::from(1.0) / scalar)
    }
}

impl Neg for &Matrix {
    type Output = Matrix;

    fn neg(self) -> Matrix {
        self * Complex64::from(-1.0)
    }
}

// Implementing fmt::Display for Matrix
impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Convert each Complex64 to string before formatting
        let string_rows: Vec<Vec<String>> = self
            .rows
            .iter()
            .map(|row| row.iter().map(complex_to_string).collect())
            .collect();

        // Use your matrix_format to format the rows
        write!(f, "{}", matrix_format(&string_rows))
    }
}

pub fn matrix_format<T: ToString>(matrix: &[Vec<T>]) -> String {
    if matrix.is_empty() {
        return String::from("[]");
    }

    let longest_row = matrix.iter().map(|v| v.len()).max().unwrap_or(0);
    let rows = matrix.len();

    // Convert the matrix elements to strings
    let string_matrix: Vec<Vec<String>> = matrix
        .iter()
        .map(|row| row.iter().map(|element| element.to_string()).collect())
        .collect();

    // Calculate the maximum width of each column
    let mut max_col_widths = Vec::with_capacity(longest_row);
    for col in 0..longest_row {
        let col_widths: Vec<usize> = string_matrix
            .iter()
            .map(|row| {
                if let Some(element) = row.get(col) {
                    element.len()
                } else {
                    0
                }
            })
            .collect();
        let max = *col_widths.iter().max().unwrap_or(&0);
        max_col_widths.push(max);
    }

    let mut formatted_string = String::from("[");
    for (row_idx, row) in string_matrix.iter().enumerate() {
        if row.is_empty() {
            continue;
        }
        // Create a vector to hold formatted elements for the row
        let formatted_row: Vec<String> = row
            .iter()
            .enumerate()
            .map(|(i, element)| {
                let width = max_col_widths[i];
                format!("{:>width$}", element, width = width)
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
        } else {
            // TODO - extend to end
        }
        formatted_string.push_str(&formatted_row_string);
    }
    formatted_string.push(']');
    formatted_string
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use num_complex::Complex64;

    use crate::test_utils::close_enough;

    use super::*;

    #[test]
    fn test_from_rows() {
        let valid_cases = vec![
            (
                vec![vec![Complex64::zero()]],
                Matrix {
                    rows: vec![vec![Complex64::zero()]],
                },
            ),
            (
                vec![vec![Complex64::new(1.0, 0.0), Complex64::new(2.0, 0.0)]],
                Matrix {
                    rows: vec![vec![Complex64::new(1.0, 0.0), Complex64::new(2.0, 0.0)]],
                },
            ),
            (
                vec![
                    vec![Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
                    vec![Complex64::new(0.0, 0.0), Complex64::new(1.0, 0.0)],
                ],
                Matrix {
                    rows: vec![
                        vec![Complex64::new(1.0, 0.0), Complex64::new(0.0, 0.0)],
                        vec![Complex64::new(0.0, 0.0), Complex64::new(1.0, 0.0)],
                    ],
                },
            ),
        ];

        for (input_rows, expected_matrix) in valid_cases {
            let matrix = Matrix::from_rows(input_rows.clone());
            assert_eq!(matrix, expected_matrix);
        }

        let invalid_cases = vec![
            // No rows provided
            vec![],
            // Row of empty columns provided
            vec![vec![], vec![]],
            // Uneven sized rows
            vec![vec![], vec![Complex64::zero()]],
        ];
        for rows in invalid_cases {
            assert!(std::panic::catch_unwind(|| Matrix::from_rows(rows)).is_err())
        }
    }

    #[test]
    fn test_identity() {
        for (input, expected) in [
            (
                1,
                Matrix {
                    rows: vec![vec![Complex64::from(1.0)]],
                },
            ),
            (
                2,
                Matrix {
                    rows: vec![
                        vec![Complex64::from(1.0), Complex64::zero()],
                        vec![Complex64::zero(), Complex64::from(1.0)],
                    ],
                },
            ),
            (
                3,
                Matrix {
                    rows: vec![
                        vec![Complex64::from(1.0), Complex64::zero(), Complex64::zero()],
                        vec![Complex64::zero(), Complex64::from(1.0), Complex64::zero()],
                        vec![Complex64::zero(), Complex64::zero(), Complex64::from(1.0)],
                    ],
                },
            ),
        ] {
            assert_eq!(Matrix::identity(input), expected)
        }
    }

    #[test]
    fn test_transpose() {
        // 1 size matrix - unchanged
        let one_size_matrix = Matrix::from_rows(vec![vec![Complex64::zero()]]);
        assert_eq!(one_size_matrix.transpose(), one_size_matrix);
        // Identity matrix - unchanged
        let identity_2x2 = Matrix::from_rows(vec![
            vec![Complex64::from(1.0), Complex64::zero()],
            vec![Complex64::zero(), Complex64::from(1.0)],
        ]);
        assert_eq!(identity_2x2.transpose(), identity_2x2);

        // Test case with more rows than columns
        let matrix_3x2 = Matrix::from_rows(vec![
            vec![Complex64::new(1.0, 0.0), Complex64::new(2.0, 0.0)],
            vec![Complex64::new(3.0, 0.0), Complex64::new(4.0, 0.0)],
            vec![Complex64::new(5.0, 0.0), Complex64::new(6.0, 0.0)],
        ]);
        let expected_transpose_2x3 = Matrix::from_rows(vec![
            vec![
                Complex64::new(1.0, 0.0),
                Complex64::new(3.0, 0.0),
                Complex64::new(5.0, 0.0),
            ],
            vec![
                Complex64::new(2.0, 0.0),
                Complex64::new(4.0, 0.0),
                Complex64::new(6.0, 0.0),
            ],
        ]);
        assert_eq!(matrix_3x2.transpose(), expected_transpose_2x3);

        // Test case with more columns than rows
        let matrix_2x3 = Matrix::from_rows(vec![
            vec![
                Complex64::new(1.0, 0.0),
                Complex64::new(2.0, 0.0),
                Complex64::new(3.0, 0.0),
            ],
            vec![
                Complex64::new(4.0, 0.0),
                Complex64::new(5.0, 0.0),
                Complex64::new(6.0, 0.0),
            ],
        ]);
        let expected_transpose_3x2 = Matrix::from_rows(vec![
            vec![Complex64::new(1.0, 0.0), Complex64::new(4.0, 0.0)],
            vec![Complex64::new(2.0, 0.0), Complex64::new(5.0, 0.0)],
            vec![Complex64::new(3.0, 0.0), Complex64::new(6.0, 0.0)],
        ]);
        assert_eq!(matrix_2x3.transpose(), expected_transpose_3x2);
    }

    #[test]
    fn test_determinant() {
        // Valid cases
        for (input, expected) in [
            // 1x1 matrix
            (
                Matrix::from_rows(vec![vec![Complex64::from(5.0)]]),
                Complex64::from(5.0),
            ),
            // 2x2 matrix
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(3.0), Complex64::from(5.0)],
                    vec![Complex64::from(8.0), Complex64::from(-13.0)],
                ]),
                Complex64::from(-79.0),
            ),
            // 3x3 matrix
            (
                Matrix::from_rows(vec![
                    vec![
                        Complex64::from(3.0),
                        Complex64::from(5.0),
                        Complex64::from(10.0),
                    ],
                    vec![
                        Complex64::from(8.0),
                        Complex64::from(-13.0),
                        Complex64::from(1.0),
                    ],
                    vec![
                        Complex64::from(3.0),
                        Complex64::from(-25.0),
                        Complex64::from(1.0),
                    ],
                ]),
                Complex64::from(-1599.0),
            ),
            // 4x4 matrix
            (
                Matrix::from_rows(vec![
                    vec![
                        Complex64::from(3.5),
                        Complex64::from(5.0),
                        Complex64::from(10.0),
                        Complex64::from(1.0),
                    ],
                    vec![
                        Complex64::from(8.1),
                        Complex64::from(-13.0),
                        Complex64::from(1.0),
                        Complex64::from(2.0),
                    ],
                    vec![
                        Complex64::from(3.3),
                        Complex64::from(-25.0),
                        Complex64::from(1.0),
                        Complex64::from(3.0),
                    ],
                    vec![
                        Complex64::from(10.0),
                        Complex64::from(9.0),
                        Complex64::from(8.5),
                        Complex64::from(7.0),
                    ],
                ]),
                Complex64::from(-9852.7),
            ),
        ] {
            assert_eq!(input.determinant(), expected);
        }

        // Invalid cases - non square matrices
        for input in [
            Matrix::from_rows(vec![vec![Complex64::zero(), Complex64::zero()]]),
            Matrix::from_rows(vec![vec![Complex64::zero()], vec![Complex64::zero()]]),
        ] {
            assert!(std::panic::catch_unwind(|| input.determinant()).is_err())
        }
    }

    #[test]
    fn test_inverse() {
        // Valid cases
        for (input, expected) in [
            // 1x1 matrix
            (
                Matrix::from_rows(vec![vec![Complex64::from(5.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(0.2)]]),
            ),
            // 2x2 matrix
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(4.0), Complex64::from(7.0)],
                    vec![Complex64::from(2.0), Complex64::from(6.0)],
                ]),
                Matrix::from_rows(vec![
                    vec![Complex64::from(0.6), Complex64::from(-0.7)],
                    vec![Complex64::from(-0.2), Complex64::from(0.4)],
                ]),
            ),
            // 3x3 matrix
            (
                Matrix::from_rows(vec![
                    vec![
                        Complex64::from(1.0),
                        Complex64::from(2.0),
                        Complex64::from(3.0),
                    ],
                    vec![
                        Complex64::from(0.0),
                        Complex64::from(1.0),
                        Complex64::from(4.0),
                    ],
                    vec![
                        Complex64::from(5.0),
                        Complex64::from(6.0),
                        Complex64::from(0.0),
                    ],
                ]),
                Matrix::from_rows(vec![
                    vec![
                        Complex64::from(-24.0),
                        Complex64::from(18.0),
                        Complex64::from(5.0),
                    ],
                    vec![
                        Complex64::from(20.0),
                        Complex64::from(-15.0),
                        Complex64::from(-4.0),
                    ],
                    vec![
                        Complex64::from(-5.0),
                        Complex64::from(4.0),
                        Complex64::from(1.0),
                    ],
                ]),
            ),
        ] {
            let inverse = input.inverse().unwrap();
            let size = inverse.rows();
            for i in 0..size {
                for j in 0..size {
                    assert!(close_enough(inverse.get(i, j), expected.get(i, j)))
                }
            }
        }

        // Singular (non-invertible) matrices - should return None
        for input in [
            // 2x2 singular matrix
            Matrix::from_rows(vec![
                vec![Complex64::from(4.0), Complex64::from(2.0)],
                vec![Complex64::from(2.0), Complex64::from(1.0)],
            ]),
            // 3x3 singular matrix
            Matrix::from_rows(vec![
                vec![
                    Complex64::from(1.0),
                    Complex64::from(2.0),
                    Complex64::from(3.0),
                ],
                vec![
                    Complex64::from(2.0),
                    Complex64::from(4.0),
                    Complex64::from(6.0),
                ],
                vec![
                    Complex64::from(3.0),
                    Complex64::from(6.0),
                    Complex64::from(9.0),
                ],
            ]),
        ] {
            assert_eq!(input.inverse(), None);
        }

        // Invalid cases - non square matrices
        for input in [
            Matrix::from_rows(vec![vec![Complex64::zero(), Complex64::zero()]]),
            Matrix::from_rows(vec![vec![Complex64::zero()], vec![Complex64::zero()]]),
        ] {
            assert!(std::panic::catch_unwind(|| input.inverse()).is_err())
        }
    }

    #[test]
    fn test_addition() {
        let valid_cases = vec![
            // Adding two 1x1 matrices
            (
                Matrix::from_rows(vec![vec![Complex64::from(1.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(2.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(3.0)]]),
            ),
            // Adding two 2x2 matrices
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
                Matrix::from_rows(vec![
                    vec![Complex64::from(5.0), Complex64::from(6.0)],
                    vec![Complex64::from(7.0), Complex64::from(8.0)],
                ]),
                Matrix::from_rows(vec![
                    vec![Complex64::from(6.0), Complex64::from(8.0)],
                    vec![Complex64::from(10.0), Complex64::from(12.0)],
                ]),
            ),
            // Adding two 2x1 matrices
            (
                Matrix::from_rows(vec![vec![Complex64::from(1.0)], vec![Complex64::from(2.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(3.0)], vec![Complex64::from(4.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(4.0)], vec![Complex64::from(6.0)]]),
            ),
            // Adding two 1x3 matrices
            (
                Matrix::from_rows(vec![vec![
                    Complex64::from(1.0),
                    Complex64::from(2.0),
                    Complex64::from(3.0),
                ]]),
                Matrix::from_rows(vec![vec![
                    Complex64::from(4.0),
                    Complex64::from(5.0),
                    Complex64::from(6.0),
                ]]),
                Matrix::from_rows(vec![vec![
                    Complex64::from(5.0),
                    Complex64::from(7.0),
                    Complex64::from(9.0),
                ]]),
            ),
        ];

        for (matrix1, matrix2, expected) in valid_cases {
            assert_eq!(&matrix1 + &matrix2, expected);
        }

        // Invalid cases - different sizes
        let invalid_cases = vec![(
            Matrix::from_rows(vec![vec![Complex64::zero()]]),
            Matrix::from_rows(vec![vec![Complex64::zero(), Complex64::zero()]]),
        )];

        for (matrix1, matrix2) in invalid_cases {
            assert!(std::panic::catch_unwind(|| &matrix1 + &matrix2).is_err())
        }
    }

    #[test]
    fn test_sub() {
        let valid_cases = vec![
            // Subtracting two 1x1 matrices
            (
                Matrix::from_rows(vec![vec![Complex64::from(3.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(2.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(1.0)]]),
            ),
            // Subtracting two 2x2 matrices
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(5.0), Complex64::from(6.0)],
                    vec![Complex64::from(7.0), Complex64::from(8.0)],
                ]),
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
                Matrix::from_rows(vec![
                    vec![Complex64::from(4.0), Complex64::from(4.0)],
                    vec![Complex64::from(4.0), Complex64::from(4.0)],
                ]),
            ),
            // Subtracting two 2x1 matrices
            (
                Matrix::from_rows(vec![vec![Complex64::from(5.0)], vec![Complex64::from(6.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(3.0)], vec![Complex64::from(2.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(2.0)], vec![Complex64::from(4.0)]]),
            ),
            // Subtracting two 1x3 matrices
            (
                Matrix::from_rows(vec![vec![
                    Complex64::from(7.0),
                    Complex64::from(8.0),
                    Complex64::from(9.0),
                ]]),
                Matrix::from_rows(vec![vec![
                    Complex64::from(1.0),
                    Complex64::from(2.0),
                    Complex64::from(3.0),
                ]]),
                Matrix::from_rows(vec![vec![
                    Complex64::from(6.0),
                    Complex64::from(6.0),
                    Complex64::from(6.0),
                ]]),
            ),
        ];

        for (matrix1, matrix2, expected) in valid_cases {
            assert_eq!(&matrix1 - &matrix2, expected);
        }

        // Invalid cases - different sizes
        let invalid_cases = vec![(
            Matrix::from_rows(vec![vec![Complex64::zero()]]), // 1x1
            Matrix::from_rows(vec![vec![Complex64::zero(), Complex64::zero()]]), // 1x2
        )];

        for (matrix1, matrix2) in invalid_cases {
            assert!(std::panic::catch_unwind(|| &matrix1 - &matrix2).is_err());
        }
    }

    #[test]
    fn test_mul_matrix() {
        // Case 1: Multiplying two same-sized matrices (2x2)
        let matrix_a = Matrix::from_rows(vec![
            vec![Complex64::from(1.0), Complex64::from(2.0)],
            vec![Complex64::from(3.0), Complex64::from(4.0)],
        ]); // 2x2

        let matrix_b = Matrix::from_rows(vec![
            vec![Complex64::from(5.0), Complex64::from(6.0)],
            vec![Complex64::from(7.0), Complex64::from(8.0)],
        ]); // 2x2

        let expected_result = Matrix::from_rows(vec![
            vec![Complex64::from(19.0), Complex64::from(22.0)],
            vec![Complex64::from(43.0), Complex64::from(50.0)],
        ]); // 2x2

        assert_eq!(&matrix_a * &matrix_b, expected_result);

        // Case 2: First matrix has more rows than columns (3x2) x (2x2)
        let matrix_a = Matrix::from_rows(vec![
            vec![Complex64::from(1.0), Complex64::from(2.0)],
            vec![Complex64::from(3.0), Complex64::from(4.0)],
            vec![Complex64::from(5.0), Complex64::from(6.0)],
        ]); // 3x2

        let matrix_b = Matrix::from_rows(vec![
            vec![Complex64::from(7.0), Complex64::from(8.0)],
            vec![Complex64::from(9.0), Complex64::from(10.0)],
        ]); // 2x2

        let expected_result = Matrix::from_rows(vec![
            vec![Complex64::from(25.0), Complex64::from(28.0)],
            vec![Complex64::from(57.0), Complex64::from(64.0)],
            vec![Complex64::from(89.0), Complex64::from(100.0)],
        ]); // 3x2

        assert_eq!(&matrix_a * &matrix_b, expected_result);

        // Case 3: Second matrix has more rows than columns (2x2) x (2x3)
        let matrix_a = Matrix::from_rows(vec![
            vec![Complex64::from(1.0), Complex64::from(2.0)],
            vec![Complex64::from(3.0), Complex64::from(4.0)],
        ]); // 2x2

        let matrix_b = Matrix::from_rows(vec![
            vec![
                Complex64::from(5.0),
                Complex64::from(6.0),
                Complex64::from(7.0),
            ],
            vec![
                Complex64::from(8.0),
                Complex64::from(9.0),
                Complex64::from(10.0),
            ],
        ]); // 2x3

        let expected_result = Matrix::from_rows(vec![
            vec![
                Complex64::from(21.0),
                Complex64::from(24.0),
                Complex64::from(27.0),
            ],
            vec![
                Complex64::from(47.0),
                Complex64::from(54.0),
                Complex64::from(61.0),
            ],
        ]); // 2x3

        assert_eq!(&matrix_a * &matrix_b, expected_result);

        // Case 4: 1x1 * 1x1
        let matrix_a = Matrix::from_rows(vec![vec![Complex64::from(5.0)]]);
        let matrix_b = Matrix::from_rows(vec![vec![Complex64::from(2.0)]]);
        let expected_result = Matrix::from_rows(vec![vec![Complex64::from(10.0)]]);
        assert_eq!(&matrix_a * &matrix_b, expected_result);

        // Case 5: Can't multiply - invalid dimensions
        let matrix_a = Matrix::from_rows(vec![vec![Complex64::zero(), Complex64::zero()]]);
        let matrix_b = matrix_a.clone();
        assert!(std::panic::catch_unwind(|| &matrix_a * &matrix_b).is_err());
    }

    #[test]
    fn test_mul_scalar() {
        let valid_cases = vec![
            // Normal cases
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
                Complex64::from(2.0),
                Matrix::from_rows(vec![
                    vec![Complex64::from(2.0), Complex64::from(4.0)],
                    vec![Complex64::from(6.0), Complex64::from(8.0)],
                ]),
            ),
            // Multiply by zero
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
                Complex64::from(0.0),
                Matrix::from_rows(vec![
                    vec![Complex64::from(0.0), Complex64::from(0.0)],
                    vec![Complex64::from(0.0), Complex64::from(0.0)],
                ]),
            ),
            //  Multiply by one
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
                Complex64::from(1.0),
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
            ),
            // Multiply by negative
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
                Complex64::from(-1.0),
                Matrix::from_rows(vec![
                    vec![Complex64::from(-1.0), Complex64::from(-2.0)],
                    vec![Complex64::from(-3.0), Complex64::from(-4.0)],
                ]),
            ),
        ];

        for (matrix, scalar, expected) in valid_cases {
            assert_eq!(scalar * &matrix, expected);
            assert_eq!(&matrix * scalar, expected);
        }
    }

    #[test]
    fn test_div_scalar() {
        let valid_cases = vec![
            // Normal cases
            (
                Matrix::from_rows(vec![
                    vec![Complex64::from(2.0), Complex64::from(4.0)],
                    vec![Complex64::from(6.0), Complex64::from(8.0)],
                ]),
                Complex64::from(2.0),
                Matrix::from_rows(vec![
                    vec![Complex64::from(1.0), Complex64::from(2.0)],
                    vec![Complex64::from(3.0), Complex64::from(4.0)],
                ]),
            ),
        ];

        for (matrix, scalar, expected) in valid_cases {
            assert_eq!(&matrix / scalar, expected);
        }
    }

    #[test]
    fn test_row_cross() {
        let row_vector1 = Matrix::from_rows(vec![vec![
            Complex64::from(1.0),
            Complex64::from(2.0),
            Complex64::from(3.0),
        ]]); // 1x3
        let row_vector2 = Matrix::from_rows(vec![vec![
            Complex64::from(4.0),
            Complex64::from(5.0),
            Complex64::from(6.0),
        ]]); // 1x3

        let result = row_vector1.row_cross(&row_vector2);
        let expected = Matrix::from_rows(vec![vec![
            Complex64::from(-3.0),
            Complex64::from(6.0),
            Complex64::from(-3.0),
        ]]);

        assert_eq!(result, expected);
    }

    #[test]
    fn test_column_cross() {
        let column_vector1 = Matrix::from_rows(vec![
            vec![Complex64::from(1.0)],
            vec![Complex64::from(2.0)],
            vec![Complex64::from(3.0)],
        ]); // 3x1

        let column_vector2 = Matrix::from_rows(vec![
            vec![Complex64::from(4.0)],
            vec![Complex64::from(5.0)],
            vec![Complex64::from(6.0)],
        ]); // 3x1

        let result = column_vector1.column_cross(&column_vector2);
        let expected = Matrix::from_rows(vec![vec![
            Complex64::from(-3.0),
            Complex64::from(6.0),
            Complex64::from(-3.0),
        ]]);

        assert_eq!(result, expected);
    }

    #[test]
    fn test_row_dot() {
        let row_vector1 = Matrix::from_rows(vec![vec![
            Complex64::from(1.0),
            Complex64::from(2.0),
            Complex64::from(3.0),
        ]]); // 1x3
        let row_vector2 = Matrix::from_rows(vec![vec![
            Complex64::from(4.0),
            Complex64::from(5.0),
            Complex64::from(6.0),
        ]]); // 1x3

        let result = row_vector1.row_dot(&row_vector2);
        let expected = Complex64::from(32.0);

        assert_eq!(result, expected);
    }

    #[test]
    fn test_column_dot() {
        let column_vector1 = Matrix::from_rows(vec![
            vec![Complex64::from(1.0)],
            vec![Complex64::from(2.0)],
            vec![Complex64::from(3.0)],
        ]); // 3x1

        let column_vector2 = Matrix::from_rows(vec![
            vec![Complex64::from(4.0)],
            vec![Complex64::from(5.0)],
            vec![Complex64::from(6.0)],
        ]); // 3x1

        let result = column_vector1.column_dot(&column_vector2);
        let expected = Complex64::from(32.0); // (1*4 + 2*5 + 3*6)

        assert_eq!(result, expected);
    }

    #[test]
    fn test_negate() {
        for (input, expected) in [
            (
                Matrix::from_rows(vec![vec![Complex64::from(1.0)]]),
                Matrix::from_rows(vec![vec![Complex64::from(-1.0)]]),
            ),
            (
                Matrix::from_rows(vec![vec![Complex64::new(1.0, 2.0), Complex64::from(0.0)]]),
                Matrix::from_rows(vec![vec![
                    Complex64::new(-1.0, -2.0),
                    Complex64::from(-0.0),
                ]]),
            ),
            (
                Matrix::from_rows(vec![
                    vec![Complex64::new(-1.23, -2.017)],
                    vec![Complex64::new(-0.0, -0.5)],
                ]),
                Matrix::from_rows(vec![
                    vec![Complex64::new(1.23, 2.017)],
                    vec![Complex64::new(0.0, 0.5)],
                ]),
            ),
        ] {
            assert_eq!(-&input, expected);
        }
    }

    #[test]
    fn test_matrix_format() {
        for (input, result) in [
            (vec![], "[]"),
            (vec![vec![]], "[]"),
            (vec![vec![], vec![]], "[]"),
            (vec![vec!["1111"]], "[1111]"),
            (vec![vec!["1", "2", "3"]], "[1, 2, 3]"),
            (
                vec![
                    vec!["1", "1233333", "3"],
                    vec!["123", "44", "3"],
                    vec!["1", "2", "three"],
                ],
                "[  1, 1233333,     3\n 123,      44,     3\n   1,       2, three]",
            ),
            (vec![vec!["1", "2"], vec!["3"]], "[1, 2\n 3]"),
        ] {
            assert_eq!(matrix_format(&input), result)
        }
    }

    #[test]
    fn test_matrix_display() {
        // Test Case 1: 1x1 matrix
        let matrix_1x1 = Matrix::from_rows(vec![vec![Complex64::new(3.0203, 2.3456)]]);
        let expected_output_1x1 = "[3.0203 + 2.3456i]";
        assert_eq!(format!("{}", matrix_1x1), expected_output_1x1);

        // Test Case 2: 2x2 matrix with real and imaginary numbers
        let matrix_2x2 = Matrix::from_rows(vec![
            vec![Complex64::new(1.0, -1.0), Complex64::new(2.0, 0.5)],
            vec![Complex64::new(-1.5, 2.5), Complex64::new(3.0, -0.75)],
        ]);
        let expected_output_2x2 = "[      1 - i,  2 + 0.5i\n -1.5 + 2.5i, 3 - 0.75i]";
        assert_eq!(format!("{}", matrix_2x2), expected_output_2x2);

        // Test Case 3: 3x3 matrix with various decimal values and complex numbers
        let matrix_3x3 = Matrix::from_rows(vec![
            vec![
                Complex64::new(0.1, 0.0),
                Complex64::new(1.234, -5.678),
                Complex64::new(-9.8765, 3.2222),
            ],
            vec![
                Complex64::new(2.0, 2.0),
                Complex64::new(-3.5, -4.25),
                Complex64::new(1.1, -1.1),
            ],
            vec![
                Complex64::new(4.0, -4.0),
                Complex64::new(0.0, 0.0),
                Complex64::new(-6.66, 6.66),
            ],
        ]);
        let expected_output_3x3 = "[   0.1, 1.234 - 5.678i, -9.8765 + 3.2222i\n 2 + 2i,   -3.5 - 4.25i,        1.1 - 1.1i\n 4 - 4i,              0,     -6.66 + 6.66i]";
        assert_eq!(format!("{}", matrix_3x3), expected_output_3x3);
    }
}
