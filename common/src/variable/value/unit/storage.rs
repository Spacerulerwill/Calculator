use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StorageUnit {
    // Bytes
    Byte,
    // Bytes (Base 10)
    Kilobyte,
    Megabyte,
    Gigabyte,
    Terabyte,
    Petabyte,
    Exabyte,
    // Bytes (Base 2)
    Kibibyte,
    Mebibyte,
    Gibibyte,
    Tebibyte,
    Petibyte,
    Exbibyte,
    // Bits
    Bit,
    // Bits (Base 10)
    Kilobit,
    Megabit,
    Gigabit,
    Terabit,
    Petabit,
    Exabit,
    // Bits (Base 2)
    Kibibit,
    Mebibit,
    Gibibit,
    Tebibit,
    Petibit,
    Exbibit,
}

impl fmt::Display for StorageUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StorageUnit::Byte => write!(f, "B"),
            StorageUnit::Kilobyte => write!(f, "KB"),
            StorageUnit::Megabyte => write!(f, "MB"),
            StorageUnit::Gigabyte => write!(f, "GB"),
            StorageUnit::Terabyte => write!(f, "TB"),
            StorageUnit::Petabyte => write!(f, "PB"),
            StorageUnit::Exabyte => write!(f, "EB"),
            StorageUnit::Kibibyte => write!(f, "KiB"),
            StorageUnit::Mebibyte => write!(f, "MiB"),
            StorageUnit::Gibibyte => write!(f, "GiB"),
            StorageUnit::Tebibyte => write!(f, "TiB"),
            StorageUnit::Petibyte => write!(f, "PiB"),
            StorageUnit::Exbibyte => write!(f, "EiB"),
            StorageUnit::Bit => write!(f, "b"),
            StorageUnit::Kilobit => write!(f, "Kb"),
            StorageUnit::Megabit => write!(f, "Mb"),
            StorageUnit::Gigabit => write!(f, "Gb"),
            StorageUnit::Terabit => write!(f, "Tb"),
            StorageUnit::Petabit => write!(f, "Pb"),
            StorageUnit::Exabit => write!(f, "Eb"),
            StorageUnit::Kibibit => write!(f, "Kib"),
            StorageUnit::Mebibit => write!(f, "Mib"),
            StorageUnit::Gibibit => write!(f, "Gib"),
            StorageUnit::Tebibit => write!(f, "Tib"),
            StorageUnit::Petibit => write!(f, "Pib"),
            StorageUnit::Exbibit => write!(f, "Eib"),
        }
    }
}

impl StorageUnit {
    /// How many of this unit there are per byte
    pub fn get_per_byte(&self) -> f64 {
        match self {
            StorageUnit::Byte => 1.0,
            StorageUnit::Kilobyte => 1e-3,
            StorageUnit::Megabyte => 1e-6,
            StorageUnit::Gigabyte => 1e-9,
            StorageUnit::Terabyte => 1e-12,
            StorageUnit::Petabyte => 1e-15,
            StorageUnit::Exabyte => 1e-18,
            StorageUnit::Kibibyte => 2_f64.powi(-10),
            StorageUnit::Mebibyte => 2_f64.powi(-20),
            StorageUnit::Gibibyte => 2_f64.powi(-30),
            StorageUnit::Tebibyte => 2_f64.powi(-40),
            StorageUnit::Petibyte => 2_f64.powi(-50),
            StorageUnit::Exbibyte => 2_f64.powi(-60),
            StorageUnit::Bit => 1.0 / 8.0,
            StorageUnit::Kilobit => 1e-3 / 8.0,
            StorageUnit::Megabit => 1e-6 / 8.0,
            StorageUnit::Gigabit => 1e-9 / 8.0,
            StorageUnit::Terabit => 1e-12 / 8.0,
            StorageUnit::Petabit => 1e-15 / 8.0,
            StorageUnit::Exabit => 1e-18 / 8.0,
            StorageUnit::Kibibit => 2_f64.powi(-10) / 8.0,
            StorageUnit::Mebibit => 2_f64.powi(-20) / 8.0,
            StorageUnit::Gibibit => 2_f64.powi(-30) / 8.0,
            StorageUnit::Tebibit => 2_f64.powi(-40) / 8.0,
            StorageUnit::Petibit => 2_f64.powi(-50) / 8.0,
            StorageUnit::Exbibit => 2_f64.powi(-60) / 8.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_storage_unit() {
        for (unit, expected) in [
            (StorageUnit::Byte, "B"),
            (StorageUnit::Kilobyte, "KB"),
            (StorageUnit::Megabyte, "MB"),
            (StorageUnit::Gigabyte, "GB"),
            (StorageUnit::Terabyte, "TB"),
            (StorageUnit::Petabyte, "PB"),
            (StorageUnit::Exabyte, "EB"),
            (StorageUnit::Kibibyte, "KiB"),
            (StorageUnit::Mebibyte, "MiB"),
            (StorageUnit::Gibibyte, "GiB"),
            (StorageUnit::Tebibyte, "TiB"),
            (StorageUnit::Petibyte, "PiB"),
            (StorageUnit::Exbibyte, "EiB"),
            (StorageUnit::Bit, "b"),
            (StorageUnit::Kilobit, "Kb"),
            (StorageUnit::Megabit, "Mb"),
            (StorageUnit::Gigabit, "Gb"),
            (StorageUnit::Terabit, "Tb"),
            (StorageUnit::Petabit, "Pb"),
            (StorageUnit::Exabit, "Eb"),
            (StorageUnit::Kibibit, "Kib"),
            (StorageUnit::Mebibit, "Mib"),
            (StorageUnit::Gibibit, "Gib"),
            (StorageUnit::Tebibit, "Tib"),
            (StorageUnit::Petibit, "Pib"),
            (StorageUnit::Exbibit, "Eib"),
        ] {
            assert_eq!(format!("{}", unit), expected);
        }
    }

    #[test]
    fn test_get_per_byte() {
        let units = vec![
            (StorageUnit::Byte, 1.0),
            (StorageUnit::Kilobyte, 1e-3),
            (StorageUnit::Megabyte, 1e-6),
            (StorageUnit::Gigabyte, 1e-9),
            (StorageUnit::Terabyte, 1e-12),
            (StorageUnit::Petabyte, 1e-15),
            (StorageUnit::Exabyte, 1e-18),
            (StorageUnit::Kibibyte, 2_f64.powi(-10)),
            (StorageUnit::Mebibyte, 2_f64.powi(-20)),
            (StorageUnit::Gibibyte, 2_f64.powi(-30)),
            (StorageUnit::Tebibyte, 2_f64.powi(-40)),
            (StorageUnit::Petibyte, 2_f64.powi(-50)),
            (StorageUnit::Exbibyte, 2_f64.powi(-60)),
            (StorageUnit::Bit, 1.0 / 8.0),
            (StorageUnit::Kilobit, 1e-3 / 8.0),
            (StorageUnit::Megabit, 1e-6 / 8.0),
            (StorageUnit::Gigabit, 1e-9 / 8.0),
            (StorageUnit::Terabit, 1e-12 / 8.0),
            (StorageUnit::Petabit, 1e-15 / 8.0),
            (StorageUnit::Exabit, 1e-18 / 8.0),
            (StorageUnit::Kibibit, 2_f64.powi(-10) / 8.0),
            (StorageUnit::Mebibit, 2_f64.powi(-20) / 8.0),
            (StorageUnit::Gibibit, 2_f64.powi(-30) / 8.0),
            (StorageUnit::Tebibit, 2_f64.powi(-40) / 8.0),
            (StorageUnit::Petibit, 2_f64.powi(-50) / 8.0),
            (StorageUnit::Exbibit, 2_f64.powi(-60) / 8.0),
        ];

        for (unit, expected) in units {
            assert!((unit.get_per_byte() - expected).abs() < 1e-12);
        }
    }
}
