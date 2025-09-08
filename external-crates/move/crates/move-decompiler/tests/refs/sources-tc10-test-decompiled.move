module 0x12::tc10 {
    public fun foo() : u64 {
        let v0 = 0;
        let v1 = v0 + 1;
        let mut v2 = if (v1 > 1) {
            let mut v3 = v1 + 1;
            while (v3 < 5) {
                v3 = v3 + 1;
            };
            v3 - 1
        } else {
            let mut v3 = v1 + 2;
            while (v3 < 6) {
                v3 = v3 + 1;
            };
            v3 - 2
        };
        v0 + v2 + 99
    }

    // decompiled from Move bytecode v6
}
