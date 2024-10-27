use num_complex::Complex64;

const EPSILON: f64 = 1e-10;

/// Comparison of floating point values
pub fn close_enough(a: Complex64, b: Complex64) -> bool {
    (a.re - b.re).abs() < EPSILON && (a.im - b.im).abs() < EPSILON
}
