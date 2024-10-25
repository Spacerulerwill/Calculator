use num_complex::Complex64;
use std::fmt;

use crate::variable::value::complex_to_string;

use super::unit::{DistanceUnit, MassUnit, StorageUnit, TemperatureUnit, Unit};

#[derive(Debug, Clone, PartialEq)]
pub struct Measurement {
    pub num: Complex64,
    pub unit: Unit,
}

impl fmt::Display for Measurement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // If both real and imaginary parts are non-zero (i.e., complex number), wrap in brackets
        if self.num.re != 0.0 && self.num.im != 0.0 {
            write!(f, "({}){}", complex_to_string(&self.num), self.unit)
        } else {
            write!(f, "{}{}", complex_to_string(&self.num), self.unit)
        }
    }
}

impl Measurement {
    pub fn new(num: Complex64, unit: Unit) -> Self {
        Self { num, unit }
    }
    
    pub fn to_other_unit(&self, unit: Unit) -> Result<Self, ()> {
        if std::mem::discriminant(&self.unit) != std::mem::discriminant(&unit) {
            return Err(());
        }
        // Convert to SI base unit (Kelvin for temperature, meters for distance, kilograms for mass)
        let si_value = self.to_si_base_unit();

        match unit {
            Unit::Distance(distance_unit) => Ok(Self::new(
                si_value.num * distance_unit.get_per_meter(),
                unit,
            )),
            Unit::Mass(mass_unit) => Ok(Self::new(si_value.num * mass_unit.get_per_kilo(), unit)),
            Unit::Storage(storage_unit) => {
                Ok(Self::new(si_value.num * storage_unit.get_per_byte(), unit))
            }
            Unit::Temperature(temperature_unit) => {
                // Convert from Kelvin to the desired temperature unit
                let converted_value = match temperature_unit {
                    TemperatureUnit::Celsius => si_value.num - Complex64::from(273.15),
                    TemperatureUnit::Fahrenheit => {
                        (si_value.num * Complex64::from(9.0 / 5.0)) - Complex64::from(459.67)
                    }
                    TemperatureUnit::Kelvin => si_value.num,
                };

                Ok(Self::new(
                    converted_value,
                    Unit::Temperature(temperature_unit),
                ))
            }
        }
    }

    pub fn to_si_base_unit(&self) -> Self {
        match self.unit {
            // Distance conversion to meters
            Unit::Distance(distance_unit) => {
                let si_value = self.num / Complex64::from(distance_unit.get_per_meter());
                Measurement::new(si_value, Unit::Distance(DistanceUnit::Meter))
            }
            // Mass conversion to kilograms
            Unit::Mass(mass_unit) => {
                let si_value = self.num / Complex64::from(mass_unit.get_per_kilo());
                Measurement::new(si_value, Unit::Mass(MassUnit::Kilogram))
            }
            Unit::Storage(storage_unit) => {
                let si_value = self.num / Complex64::from(storage_unit.get_per_byte());
                Measurement::new(si_value, Unit::Storage(StorageUnit::Byte))
            }
            // Temperature conversion to Kelvin
            Unit::Temperature(temp_unit) => {
                let si_value = match temp_unit {
                    TemperatureUnit::Celsius => self.num + Complex64::from(273.15),
                    TemperatureUnit::Fahrenheit => {
                        (self.num + Complex64::new(459.67, 0.0)) * Complex64::new(5.0 / 9.0, 0.0)
                    }
                    TemperatureUnit::Kelvin => self.num, // Already in kelvin
                };
                Measurement::new(si_value, Unit::Temperature(TemperatureUnit::Kelvin))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_measurement() {
        for (input, expected) in [
            (
                Measurement::new(Complex64::new(1.0, 2.0), Unit::Mass(MassUnit::Kilogram)),
                "(1 + 2i)kg",
            ),
            (
                Measurement::new(Complex64::from(1.0), Unit::Distance(DistanceUnit::Centimeter)),
                "1cm"
            ),
            (
                Measurement::new(Complex64::i(), Unit::Distance(DistanceUnit::Centimeter)),
                "icm"
            ),
        ] {
            assert_eq!(input.to_string(), expected);
        }
    }

    fn close_enough(a: Complex64, b: Complex64, epsilon: f64) -> bool {
        (a.re - b.re).abs() < epsilon && (a.im - b.im).abs() < epsilon
    }
    
    #[test]
    fn test_measurement_to_si_unit() {
        let epsilon = 1e-10; // Define a small tolerance
    
        for (input, expected) in [
            // Centimeter to Meter
            (
                Measurement::new(Complex64::from(100.0), Unit::Distance(DistanceUnit::Centimeter)),
                Measurement::new(Complex64::from(1.0), Unit::Distance(DistanceUnit::Meter)),
            ),
            // Gram to Kilogram
            (
                Measurement::new(Complex64::from(1000.0), Unit::Mass(MassUnit::Gram)),
                Measurement::new(Complex64::from(1.0), Unit::Mass(MassUnit::Kilogram)),
            ),
            // Megabyte to Byte
            (
                Measurement::new(Complex64::from(6.0), Unit::Storage(StorageUnit::Megabyte)),
                Measurement::new(Complex64::from(6e6), Unit::Storage(StorageUnit::Byte)),
            ),
            // Celsius to Kelvin
            (
                Measurement::new(Complex64::from(0.0), Unit::Temperature(TemperatureUnit::Celsius)),
                Measurement::new(Complex64::from(273.15), Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
            // Kelvin to Kelvin (should return the same value)
            (
                Measurement::new(Complex64::from(273.15), Unit::Temperature(TemperatureUnit::Kelvin)),
                Measurement::new(Complex64::from(273.15), Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
            // Fahrenheit to Kelvin (32°F = 273.15K)
            (
                Measurement::new(Complex64::from(32.0), Unit::Temperature(TemperatureUnit::Fahrenheit)),
                Measurement::new(Complex64::from(273.15), Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
            // Another Fahrenheit to Kelvin conversion (212°F = 373.15K)
            (
                Measurement::new(Complex64::from(212.0), Unit::Temperature(TemperatureUnit::Fahrenheit)),
                Measurement::new(Complex64::from(373.15), Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
        ] {
            let result = input.to_si_base_unit();
            assert!(close_enough(result.num, expected.num, epsilon));
            assert_eq!(result.unit, expected.unit);
        }
    }
    
    #[test]
    fn test_measurement_to_other_unit() {
        let epsilon = 1e-10;
    
        for (input, target_unit, expected) in [
            (
                Measurement::new(Complex64::from(1000.0), Unit::Distance(DistanceUnit::Meter)),
                Unit::Distance(DistanceUnit::Kilometer),
                Measurement::new(Complex64::from(1.0), Unit::Distance(DistanceUnit::Kilometer)),
            ),
            (
                Measurement::new(Complex64::from(1.0), Unit::Mass(MassUnit::Kilogram)),
                Unit::Mass(MassUnit::Gram),
                Measurement::new(Complex64::from(1000.0), Unit::Mass(MassUnit::Gram)),
            ),
            (
                Measurement::new(Complex64::from(273.15), Unit::Temperature(TemperatureUnit::Kelvin)),
                Unit::Temperature(TemperatureUnit::Celsius),
                Measurement::new(Complex64::from(0.0), Unit::Temperature(TemperatureUnit::Celsius)),
            ),
            (
                Measurement::new(Complex64::from(0.0), Unit::Temperature(TemperatureUnit::Celsius)),
                Unit::Temperature(TemperatureUnit::Fahrenheit),
                Measurement::new(Complex64::from(32.0), Unit::Temperature(TemperatureUnit::Fahrenheit)),
            ),
            (
                Measurement::new(Complex64::from(32.0), Unit::Temperature(TemperatureUnit::Fahrenheit)),
                Unit::Temperature(TemperatureUnit::Kelvin),
                Measurement::new(Complex64::from(273.15), Unit::Temperature(TemperatureUnit::Kelvin)),
            ),
            (
                Measurement::new(Complex64::from(6.0), Unit::Storage(StorageUnit::Megabyte)),
                Unit::Storage(StorageUnit::Byte),
                Measurement::new(Complex64::from(6e6), Unit::Storage(StorageUnit::Byte)),
            ),
        ] {
            let result = input.to_other_unit(target_unit).unwrap();
            assert!(close_enough(result.num, expected.num, epsilon));
            assert_eq!(result.unit, expected.unit);
        }
    
        assert_eq!(
            Measurement::new(Complex64::ZERO, Unit::Distance(DistanceUnit::Centimeter)).to_other_unit(Unit::Mass(MassUnit::Kilogram)).unwrap_err(),
            ()
        );
    }    
}
