//- splitmix64 (http://xoroshiro.di.unimi.it/splitmix64.c) 
//"""
//  Written in 2015 by Sebastiano Vigna (vigna@acm.org)
//
//  To the extent possible under law, the author has dedicated all copyright
//  and related and neighboring rights to this software to the public domain
//  worldwide. This software is distributed without any warranty.
//
//  See <http://creativecommons.org/publicdomain/zero/1.0/>. 
//"""
//
// Written by Alexander Stocko <as@coder.gg>
//
// To the extent possible under law, the author has dedicated all copyright
// and related and neighboring rights to this software to the public domain
// worldwide. This software is distributed without any warranty.
//
// See <LICENSE or http://creativecommons.org/publicdomain/zero/1.0/>

use std::num::Wrapping as w;

/// The `SplitMix64` random number generator.
#[derive(Copy, Clone)]
pub struct SplitMix64(pub u64);

impl SplitMix64 {
    pub fn new(seed: u64) -> Self { Self(seed) }
    pub fn new_from_i64(seed: i64) -> Self {
        Self::new(u64::from_be_bytes(seed.to_be_bytes()))
    }

    #[inline]
    pub fn next_u64(&mut self) -> u64 {
        let mut z = w(self.0) + w(0x9E37_79B9_7F4A_7C15_u64);
        self.0 = z.0;
        z = (z ^ (z >> 30)) * w(0xBF58_476D_1CE4_E5B9_u64);
        z = (z ^ (z >> 27)) * w(0x94D0_49BB_1331_11EB_u64);
        (z ^ (z >> 31)).0
    }

    #[inline]
    pub fn next_i64(&mut self) -> i64 {
        i64::from_be_bytes(
            self.next_u64().to_be_bytes())
    }
}

// Taken from rand::distributions
// Licensed under the Apache License, Version 2.0
// Copyright 2018 Developers of the Rand project.
pub fn u64_to_open01(u: u64) -> f64 {
    use core::f64::EPSILON;
    let float_size         = std::mem::size_of::<f64>() as u32 * 8;
    let fraction           = u >> (float_size - 52);
    let exponent_bits: u64 = (1023 as u64) << 52;
    f64::from_bits(fraction | exponent_bits) - (1.0 - EPSILON / 2.0)
}

/// Original FNV Rust implementation taken from fnv crate on
/// crates.io by Alex Crichton <alex@alexcrichton.com>
/// under Apache-2.0 / MIT license Copyright 2018.
///
/// An implementation of the Fowler–Noll–Vo hash function.
///
/// See the [crate documentation](index.html) for more details.
#[derive(Copy, Clone, Debug)]
pub struct FnvHasher(u64);

impl Default for FnvHasher {
    #[inline]
    fn default() -> FnvHasher {
        FnvHasher(0xcbf2_9ce4_8422_2325)
    }
}

impl FnvHasher {
    #[inline]
    #[allow(dead_code)]
    pub fn with_key(key: u64) -> FnvHasher {
        FnvHasher(key)
    }

    #[inline]
    fn finish(self) -> u64 {
        self.0
    }

    #[inline]
    pub fn finish_i64(self) -> i64 {
        i64::from_be_bytes(
            self.finish().to_be_bytes())
    }

    #[inline]
    pub fn write_f64(&mut self, f: f64) {
        self.write(&f.to_bits().to_be_bytes());
    }

    #[inline]
    pub fn write_i64(&mut self, i: i64) {
        self.write(&i.to_be_bytes());
    }

    #[inline]
    #[allow(clippy::cast_lossless)]
    pub fn write(&mut self, bytes: &[u8]) {
        let FnvHasher(mut hash) = *self;

        for byte in bytes.iter() {
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(0x0100_0000_01b3);
        }

        *self = FnvHasher(hash);
    }
}

pub fn now_timestamp() -> u64 {
    std::time::SystemTime::now()
    .duration_since(std::time::SystemTime::UNIX_EPOCH)
    .unwrap()
    .as_secs() as u64
}

