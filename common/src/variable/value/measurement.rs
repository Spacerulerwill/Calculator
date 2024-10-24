use num_complex::Complex64;
use std::fmt;

use crate::variable::value::complex_to_string;

use super::unit::{DistanceUnit, MassUnit, StorageUnit, TemperatureUnit, Unit};

#[derive(Debug, Clone, PartialEq)]
pub struct Measurement {
    pub num: Complex64,
    pub kind: Unit,
}

impl fmt::Display for Measurement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // If both real and imaginary parts are non-zero (i.e., complex number), wrap in brackets
        if self.num.re != 0.0 && self.num.im != 0.0 {
            write!(f, "({}){}", complex_to_string(&self.num), self.kind)
        } else {
            write!(f, "{}{}", complex_to_string(&self.num), self.kind)
        }
    }
}

impl Measurement {
    pub fn to_other_unit(&self, unit: Unit) -> Result<Self, ()> {
        if std::mem::discriminant(&self.kind) != std::mem::discriminant(&unit) {
            return Err(());
        }
        // Convert to SI base unit (Kelvin for temperature, meters for distance, kilograms for mass)
        let si_value = self.to_si_base_unit();

        match unit {
            Unit::Distance(distance_unit) => Ok(Self {
                num: si_value.num * distance_unit.get_per_meter(),
                kind: unit,
            }),
            Unit::Mass(mass_unit) => Ok(Self {
                num: si_value.num * mass_unit.get_per_kilo(),
                kind: unit,
            }),
            Unit::Storage(storage_unit) => Ok(Self {
                num: si_value.num * storage_unit.get_per_byte(),
                kind: unit,
            }),
            Unit::Temperature(temperature_unit) => {
                // Convert from Kelvin to the desired temperature unit
                let converted_value = match temperature_unit {
                    TemperatureUnit::Celsius => si_value.num - Complex64::from(273.15),
                    TemperatureUnit::Fahrenheit => {
                        (si_value.num * Complex64::from(9.0 / 5.0)) - Complex64::from(459.67)
                    }
                    TemperatureUnit::Kelvin => si_value.num,
                };

                Ok(Self {
                    num: converted_value,
                    kind: Unit::Temperature(temperature_unit),
                })
            }
        }
    }

    pub fn to_si_base_unit(&self) -> Self {
        match self.kind {
            // Distance conversion to meters
            Unit::Distance(distance_unit) => {
                let si_value = self.num / Complex64::from(distance_unit.get_per_meter());
                Measurement {
                    num: si_value,
                    kind: Unit::Distance(DistanceUnit::Meter),
                }
            }
            // Mass conversion to kilograms
            Unit::Mass(mass_unit) => {
                let si_value = self.num / Complex64::from(mass_unit.get_per_kilo());
                Measurement {
                    num: si_value,
                    kind: Unit::Mass(MassUnit::Kilogram),
                }
            }
            Unit::Storage(storage_unit) => {
                let si_value = self.num / Complex64::from(storage_unit.get_per_byte());
                Measurement {
                    num: si_value,
                    kind: Unit::Storage(StorageUnit::Byte),
                }
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
                Measurement {
                    num: si_value,
                    kind: Unit::Temperature(TemperatureUnit::Kelvin),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {}
