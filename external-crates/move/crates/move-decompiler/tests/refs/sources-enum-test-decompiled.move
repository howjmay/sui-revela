module 0x12::enum_only {
    public struct Test {
        a: u64,
        b: u64,
    }

    public enum Color has drop {
        RGB {
            red: u8,
            green: u8,
            blue: u8,
        },
        HSL {
            hue: u16,
            saturation: u8,
            lightness: u8,
        },
        Hex {
            pos0: u32,
        },
    }

    public enum Option<T0> {
        None,
        Some {
            pos0: T0,
        },
    }

    public fun hex_color(arg0: Color) : u32 {
        let v0 = arg0;
        match (v0) {
            Color::RGB { red: v5, green: v6, blue: v7 } => {
                (v5 as u32) << 16 | (v6 as u32) << 8 | (v7 as u32)
            },
            Color::HSL { .. } => {
                abort 0
            },
            Color::Hex { pos0: mut v1 } => {
                v1
            },
        }
    }

    public fun hex_color_ref(arg0: &Color) : u32 {
        match (arg0) {
            Color::RGB { red: v4, green: v5, blue: v6 } => {
                (*v4 as u32) << 16 | (*v5 as u32) << 8 | (*v6 as u32)
            },
            Color::HSL { .. } => {
                abort 0
            },
            Color::Hex { pos0: v10 } => {
                *v10
            },
        }
    }

    public fun is_rgb_color(arg0: Color) : bool {
        let v0 = arg0;
        match (v0) {
            Color::RGB { .. } => {
                true
            },
            Color::HSL { .. } => {
                false
            },
            Color::Hex { .. } => {
                false
            },
        }
    }

    public fun unwrap_some<T0>(arg0: Option<T0>) : T0 {
        let v0 = arg0;
        match (v0) {
            Option<T0>::None => {
                abort 0
            },
            Option<T0>::Some { pos0: v1 } => {
                v1
            },
        }
    }

    // decompiled from Move bytecode v7
}
